# Go パフォーマンス監視パターン

## defer を使った処理時間測定

```go
func (s *Service) ProcessRequest(ctx context.Context, req *Request) (*Response, error) {
    logger := log.FromContext(ctx)
    start := time.Now()

    defer func() {
        duration := time.Since(start)
        logger.WithFields(map[string]interface{}{
            "operation":     "process_request",
            "duration_ms":   duration.Milliseconds(),
            "request_type":  req.Type,
        }).Info("Request processing completed")
    }()

    // メイン処理...
    return processBusinessLogic(ctx, req)
}
```

## 適用場面
- 検索・DB操作等の時間がかかる処理
- パフォーマンス監視が必要なAPI
- ボトルネック特定のためのメトリクス収集

## 利点
- **自動実行**: deferにより必ず実行される
- **エラー時も計測**: panicやearly returnでも計測
- **コード整理**: 計測ロジックがメイン処理と分離

## ベストプラクティス

### runtime/pprof との連携
```go
import _ "net/http/pprof"

// main関数で起動
go func() {
    log.Println(http.ListenAndServe("localhost:6060", nil))
}()
```

### runtime統計の活用
```go
var m runtime.MemStats
runtime.ReadMemStats(&m)
logger.Info("Memory stats", 
    "alloc_mb", m.Alloc/1024/1024,
    "total_alloc_mb", m.TotalAlloc/1024/1024,
    "sys_mb", m.Sys/1024/1024,
    "num_gc", m.NumGC,
)
```

## 拡張パターン

### エラー状態も記録
```go
defer func() {
    duration := time.Since(start)
    fields := map[string]interface{}{
        "operation":   "process_request",
        "duration_ms": duration.Milliseconds(),
    }
    if err != nil {
        fields["error"] = err.Error()
        logger.WithFields(fields).Error("Request failed")
    } else {
        logger.WithFields(fields).Info("Request succeeded")
    }
}()
```

### 閾値アラート
```go
defer func() {
    duration := time.Since(start)
    if duration > 100*time.Millisecond {
        logger.Warn("Slow request detected", "duration_ms", duration.Milliseconds())
    }
}()
```