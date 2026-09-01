package pullrequest

// threadNodeFields is what a review thread is read as.
//
// One constant, embedded in the query that fetches the first page of threads
// and in the one that fetches the rest. They have to select identically: a
// field missing from the continuation would leave every thread past the first
// page with no comments and no tail, and the tail is what the reply logic
// reads — so the whole thing would go wrong quietly, on large pull requests
// only.
//
// tail is the same connection asked from the other end. The comments are
// paginated forwards, and a truncation drops the newest ones — which are
// exactly what says whether the ball is back with us. Asking for the last one
// separately keeps that right however much was cut.
const threadNodeFields = `
        id
        isResolved
        isOutdated
        path
        line
        resolvedBy { login }
        comments(first: 100) {
          totalCount
          pageInfo { hasNextPage endCursor }
          nodes { author { login } body createdAt url }
        }
        tail: comments(last: 1) { nodes { author { login } body createdAt url } }
`

// bodyQuery reads everything the first round trip can carry.
//
// The head commit is pinned by oid rather than taken from commits(last: 1), so
// that the date compared against a thread's last reply belongs to the same
// commit the freshness check is looking at.
//
// reviews are asked for from the end: they arrive oldest first, and a pull
// request with more than fifty — CI and bot reviews add up — would otherwise
// answer with a window holding none of the outstanding requests for changes.
const bodyQuery = `
query($owner: String!, $name: String!, $number: Int!, $headOid: GitObjectID!) {
  viewer { login }
  repository(owner: $owner, name: $name) {
    headCommit: object(oid: $headOid) { ... on Commit { committedDate } }
    pullRequest(number: $number) {
      comments(first: 100) {
        totalCount
        pageInfo { hasNextPage endCursor }
        nodes { author { login __typename } body createdAt url }
      }
      reviews(last: 50) {
        totalCount
        nodes { author { login } state body url submittedAt }
      }
      reviewThreads(first: 100) {
        totalCount
        pageInfo { hasNextPage endCursor }
        nodes {` + threadNodeFields + `      }
      }
    }
  }
}`

const commentsPageQuery = `
query($owner: String!, $name: String!, $number: Int!, $cursor: String!) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      comments(first: 100, after: $cursor) {
        pageInfo { hasNextPage endCursor }
        nodes { author { login __typename } body createdAt url }
      }
    }
  }
}`

const threadsPageQuery = `
query($owner: String!, $name: String!, $number: Int!, $cursor: String!) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      reviewThreads(first: 100, after: $cursor) {
        pageInfo { hasNextPage endCursor }
        nodes {` + threadNodeFields + `      }
      }
    }
  }
}`

// threadCommentsPageQuery follows one thread's comments, which cannot be
// reached from the pull request: each thread has its own cursor, so they are
// walked one at a time by node id.
const threadCommentsPageQuery = `
query($threadId: ID!, $cursor: String!) {
  node(id: $threadId) {
    ... on PullRequestReviewThread {
      comments(first: 100, after: $cursor) {
        pageInfo { hasNextPage endCursor }
        nodes { author { login } body createdAt url }
      }
    }
  }
}`
