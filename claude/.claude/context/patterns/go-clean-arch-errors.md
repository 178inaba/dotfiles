# Go Clean Architecture エラーハンドリングパターン

Clean Architecture（Handler/UseCase/Repository）でのエラーハンドリング設計パターン。

## 基本原則

| 層 | 責務 | エラーの扱い |
|---|---|---|
| **Repository** | DB/外部APIアクセス | センチネルエラーを返す |
| **UseCase** | ビジネスロジック | エラーをそのまま伝播 |
| **Handler** | HTTP req/res変換 | `errors.Is()`で判定し、HTTPステータスに変換 |

## 設計判断の根拠

### 1. Repository層でセンチネルエラーを返す理由

**「見つからない」はデータアクセスの結果であり、Repository層が最初にその事実を知る**

- 内部実装詳細（`sql.ErrNoRows`）を外部に漏らさない
- UseCase層のnilチェックが不要になる
- 全層で`errors.Is()`による一貫した判定が可能

### 2. Handler層でHTTPステータスコードに変換する理由

**UseCase層はトランスポート（HTTP/gRPC/CLI）を意識すべきでない**

- UseCase層はHTTP固有の概念（ステータスコード）を知らない
- 同じUseCaseを異なるトランスポートで再利用可能

### 3. ログ出力の原則

**400系はクライアントエラー、500系はサーバーエラー**

| HTTPステータス | ログ | 理由 |
|---------------|------|------|
| 400 Bad Request | なし | リクエスト形式エラー |
| 401 Unauthorized | なし | 認証エラー |
| 404 Not Found | なし | リソースが存在しない |
| 409 Conflict | なし | ビジネスルール違反 |
| 422 Unprocessable Entity | なし | バリデーションエラー |
| 500 Internal Server Error | **あり** | 調査が必要 |

## 実装パターン

### Entity層: センチネルエラー定義

```go
// entity/errors.go
var (
    ErrUserNotFound    = errors.New("user not found")
    ErrOrderNotFound   = errors.New("order not found")
    ErrProductNotFound = errors.New("product not found")
    // 409 Conflict用
    ErrDuplicateEntry  = errors.New("duplicate entry")
)
```

### Repository層: センチネルエラーを返す

```go
func (r *UserRepository) GetByID(ctx context.Context, id int) (*entity.User, error) {
    var u entity.User
    if err := r.db.GetContext(ctx, &u, query, id); err != nil {
        if errors.Is(err, sql.ErrNoRows) {
            return nil, entity.ErrUserNotFound  // センチネルエラー
        }
        return nil, fmt.Errorf("get: %w", err)
    }
    return &u, nil
}

// Listメソッドは0件でもエラーではない
func (r *UserRepository) List(ctx context.Context) ([]*entity.User, error) {
    var users []*entity.User
    if err := r.db.SelectContext(ctx, &users, query); err != nil {
        return nil, fmt.Errorf("select: %w", err)
    }
    return users, nil  // 0件でもnil, nilではなく空スライス, nil
}
```

### UseCase層: エラーをそのまま伝播

```go
func (u *UserUseCase) Get(ctx context.Context, id int) (*entity.User, error) {
    user, err := u.repo.GetByID(ctx, id)
    if err != nil {
        return nil, err  // そのまま返す（ラップしない）
    }
    return user, nil
}

// nilチェックは不要（Repositoryがエラーを返すため）
```

### UseCase層: 特殊パターン

```go
// 「存在しなければ作成」パターン
func (u *UserUseCase) GetOrCreate(ctx context.Context, externalID string) (*entity.User, error) {
    user, err := u.repo.GetByExternalID(ctx, externalID)
    if errors.Is(err, entity.ErrUserNotFound) {
        return u.repo.Create(ctx, entity.User{ExternalID: externalID})
    }
    if err != nil {
        return nil, err
    }
    return user, nil
}

// 重複チェックパターン
func (u *UserUseCase) Create(ctx context.Context, user entity.User) (*entity.User, error) {
    _, err := u.repo.GetByEmail(ctx, user.Email)
    if err == nil {
        return nil, entity.ErrDuplicateEntry  // 見つかった = 重複
    }
    if !errors.Is(err, entity.ErrUserNotFound) {
        return nil, err  // 予期しないエラー
    }
    // ErrUserNotFound = 重複なし、作成続行
    return u.repo.Create(ctx, user)
}
```

### Handler層: エラー→HTTPステータス変換

```go
func (h *UserHandler) Get(c echo.Context) error {
    ctx := c.Request().Context()
    logger := c.Logger()

    var r struct {
        ID int `param:"id" validate:"required"`
    }
    // 400系: ログ出力しない
    if err := c.Bind(&r); err != nil {
        return echo.NewHTTPError(http.StatusBadRequest, "Invalid request")
    }
    if err := c.Validate(&r); err != nil {
        return err  // 422
    }

    user, err := h.useCase.Get(ctx, r.ID)
    if err != nil {
        // 404: ログ出力しない
        if errors.Is(err, entity.ErrUserNotFound) {
            return echo.NewHTTPError(http.StatusNotFound, "User not found")
        }
        // 500: ログ出力する
        logger.Errorf("Get: %v", err)
        return echo.NewHTTPError(http.StatusInternalServerError, "Internal error")
    }

    return c.JSON(http.StatusOK, newUserResponse(user))
}
```

### Handler層: JSON配列のnull対策

```go
func (h *UserHandler) List(c echo.Context) error {
    users, err := h.useCase.List(ctx)
    if err != nil {
        // エラーハンドリング
    }

    // make()で初期化し、0件でも[]を返す（nullではなく）
    responses := make([]*UserResponse, len(users))
    for i, u := range users {
        responses[i] = newUserResponse(u)
    }

    return c.JSON(http.StatusOK, ListResponse{Results: responses})
}
```

## テストパターン

### Repository層テスト

```go
func TestUserRepository_GetByID_NotFound(t *testing.T) {
    result, err := repo.GetByID(ctx, 99999)

    if !errors.Is(err, entity.ErrUserNotFound) {
        t.Errorf("expected ErrUserNotFound, got %v", err)
    }
    if result != nil {
        t.Errorf("expected nil result")
    }
}
```

### UseCase層テスト（スタブ）

```go
type userRepoStub struct {
    returnedUser  *entity.User
    returnedError error
}

func (s *userRepoStub) GetByID(ctx context.Context, id int) (*entity.User, error) {
    if s.returnedError != nil {
        return nil, s.returnedError
    }
    return s.returnedUser, nil
}

// テストケース
{
    name:          "NotFound",
    returnedError: entity.ErrUserNotFound,
    expectError:   true,
}
```

### Handler層テスト

```go
{
    name:          "NotFoundError",
    returnedError: entity.ErrUserNotFound,
    expectStatus:  http.StatusNotFound,
}
```

## 参考資料

- [Stack Overflow: Clean architecture with golang - Store Error handling](https://stackoverflow.com/questions/77654543/clean-architecture-with-golang-store-error-handling)
- [DEV Community: Clean Architecture in Go](https://dev.to/leapcell/clean-architecture-in-go-a-practical-guide-with-go-clean-arch-51h7)
- [Stack Overflow: Return an empty array instead of null](https://stackoverflow.com/questions/56200925/return-an-empty-array-instead-of-null-with-golang-for-json-return-with-gin)
