# Go Clean Architecture パターン

Clean Architecture（Handler/UseCase/Repository）でのエラーハンドリングおよびデータフロー設計パターン。

## レイヤー間のデータフロー

### 基本原則: UseCaseはEntityを返す

```
Handler (HTTP固有)
  ├── Response型を定義（private）
  └── Entity → Response 変換
       ↓
UseCase (ビジネスロジック)
  └── Entity を返す（DTOではない）
       ↓
Repository (データアクセス)
  └── Entity を返す
```

**理由:**
- UseCase層はトランスポート（HTTP/gRPC/CLI）を意識すべきでない
- 同じUseCaseを異なるトランスポートで再利用可能
- Handler層がHTTP固有の変換（レスポンス形式）を担当

### Handler層でのレスポンス型定義

```go
// handler/user.go - レスポンス型はhandler内でprivateに定義
type userResponse struct {
    ID    int64  `json:"id"`
    Name  string `json:"name"`
    Email string `json:"email"`
}

type userListResponse struct {
    Results    []userResponse `json:"results"`
    TotalCount int            `json:"total_count"`
}

// Entity → Response 変換関数
func newUserResponse(u *entity.User) userResponse {
    return userResponse{
        ID:    u.ID,
        Name:  u.Name,
        Email: u.Email,
    }
}

func (h *UserHandler) List(c echo.Context) error {
    users, err := h.useCase.List(ctx)  // []*entity.User を返す
    if err != nil {
        // エラーハンドリング
    }

    results := make([]userResponse, len(users))
    for i, u := range users {
        results[i] = newUserResponse(u)
    }

    return c.JSON(http.StatusOK, userListResponse{
        Results:    results,
        TotalCount: len(results),
    })
}
```

### リクエスト型もHandler層で定義

リクエスト/レスポンス型はHTTP固有の概念のため、すべてHandler層で定義する。
`dto/`パッケージは不要。

```go
// handler/user.go - リクエスト/レスポンス型はhandler内でprivateに定義
type createUserRequest struct {
    Name  string `json:"name" validate:"required"`
    Email string `json:"email" validate:"required,email"`
}

type userListFilter struct {
    Page *int `query:"page"`
    Per  *int `query:"per"`
}
```

## エラーハンドリング原則

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

## Entity層の純粋性（dbタグの分離）

### 原則: Entity層はインフラ詳細を持たない

Entity層は純粋なドメインモデルとして、`db`タグなどのインフラ詳細を持たない。DBマッピングはRepository層の内部で行う。

```
Entity層（純粋なドメイン）
  └── タグなし、Goプリミティブ型
       ↓
Repository層（データアクセス）
  ├── private *Record型（dbタグ付き）
  └── entity()変換メソッド
```

**理由:**
1. **Entity層の純粋性**: ドメインモデルがインフラ詳細（DBタグ）から完全に独立
2. **Repository層が変換責務を持つ**: DBモデル↔Entityの変換はRepository内部で完結
3. **依存関係の方向が正しい**: Repository層がEntity層に依存し、逆はない

### Entity層: タグなし、Goプリミティブ型

```go
// entity/article.go - 純粋なドメインモデル
type Article struct {
    ID          int64
    AuthorID    int64
    Title       string
    Body        string
    Summary     *string    // nullable は *型
    PublishedAt *time.Time
    CreatedAt   time.Time
    UpdatedAt   time.Time
}
```

### Repository層: private Record型 + entity()変換

```go
// repository/article.go
type articleRecord struct {
    ID          int64          `db:"id"`
    AuthorID    int64          `db:"author_id"`
    Title       string         `db:"title"`
    Body        string         `db:"body"`
    Summary     sql.NullString `db:"summary"`
    PublishedAt sql.NullTime   `db:"published_at"`
    CreatedAt   time.Time      `db:"created_at"`
    UpdatedAt   time.Time      `db:"updated_at"`
}

func (r *articleRecord) entity() *entity.Article {
    a := &entity.Article{
        ID:        r.ID,
        AuthorID:  r.AuthorID,
        Title:     r.Title,
        Body:      r.Body,
        CreatedAt: r.CreatedAt,
        UpdatedAt: r.UpdatedAt,
    }
    // sql.Null* → *型 への変換
    if r.Summary.Valid {
        a.Summary = &r.Summary.String
    }
    if r.PublishedAt.Valid {
        a.PublishedAt = &r.PublishedAt.Time
    }
    return a
}
```

### Repository層: 読み取りメソッドでの使用

```go
var articleColumns = []string{
    "id", "author_id", "title", "body", "summary", "published_at", "created_at", "updated_at",
}

func (r *ArticleRepository) GetByID(ctx context.Context, id int64) (*entity.Article, error) {
    query, args, err := r.sq.Select(articleColumns...).From("articles").Where(sq.Eq{"id": id}).ToSql()
    if err != nil {
        return nil, fmt.Errorf("to sql: %w", err)
    }

    var rec articleRecord  // DBスキャンはRecord型へ
    if err := r.db.GetContext(ctx, &rec, query, args...); err != nil {
        if errors.Is(err, sql.ErrNoRows) {
            return nil, entity.ErrArticleNotFound
        }
        return nil, fmt.Errorf("get: %w", err)
    }

    return rec.entity(), nil  // Entity型に変換して返す
}

func (r *ArticleRepository) ListByAuthor(ctx context.Context, authorID int64) ([]entity.Article, error) {
    query, args, err := r.sq.Select(articleColumns...).From("articles").Where(sq.Eq{"author_id": authorID}).ToSql()
    if err != nil {
        return nil, fmt.Errorf("to sql: %w", err)
    }

    var records []articleRecord
    if err := r.db.SelectContext(ctx, &records, query, args...); err != nil {
        return nil, fmt.Errorf("select: %w", err)
    }

    articles := make([]entity.Article, len(records))
    for i, rec := range records {
        articles[i] = *rec.entity()
    }
    return articles, nil
}
```

### Repository層: 書き込みメソッドでの変換

```go
func (r *ArticleRepository) Create(ctx context.Context, article *entity.Article) error {
    // *型 → sql.Null* への変換
    var summary sql.NullString
    if article.Summary != nil {
        summary = sql.NullString{String: *article.Summary, Valid: true}
    }
    var publishedAt sql.NullTime
    if article.PublishedAt != nil {
        publishedAt = sql.NullTime{Time: *article.PublishedAt, Valid: true}
    }

    query, args, err := r.sq.Insert("articles").
        Columns("author_id", "title", "body", "summary", "published_at").
        Values(article.AuthorID, article.Title, article.Body, summary, publishedAt).
        Suffix("RETURNING id, created_at, updated_at").
        ToSql()
    if err != nil {
        return fmt.Errorf("to sql: %w", err)
    }

    return r.db.QueryRowContext(ctx, query, args...).
        Scan(&article.ID, &article.CreatedAt, &article.UpdatedAt)
}
```

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

// Update/DeleteはRowsAffectedで存在確認
func (r *UserRepository) Delete(ctx context.Context, id int) error {
    query, args, err := sq.
        Update("users").
        Set("deleted_at", sq.Expr("NOW()")).
        Where(sq.And{
            sq.Eq{"id": id},
            sq.Eq{"deleted_at": nil},  // 論理削除済みは対象外
        }).
        ToSql()
    if err != nil {
        return fmt.Errorf("to sql: %w", err)
    }

    result, err := r.db.ExecContext(ctx, query, args...)
    if err != nil {
        return fmt.Errorf("exec: %w", err)
    }

    rowsAffected, err := result.RowsAffected()
    if err != nil {
        return fmt.Errorf("rows affected: %w", err)
    }
    if rowsAffected == 0 {
        return entity.ErrUserNotFound  // センチネルエラー
    }
    return nil
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
