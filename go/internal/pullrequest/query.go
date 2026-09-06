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
        originalLine
        resolvedBy { login }
        comments(first: 100) {
          totalCount
          pageInfo { hasNextPage endCursor }
          nodes { author { login __typename } body createdAt url }
        }
        tail: ` + newestComment + `
`

// newestComment is the window that decides which comment counts as a thread's
// last one.
//
// Shared by the context's tail alias and by the liveness re-read below, because
// the staleness check is an equality between the two: the moment one of them
// means something other than "the newest comment", a thread that has been
// answered reads as unchanged.
const newestComment = `comments(last: 1) { nodes { author { login __typename } body createdAt url } }`

// bodyQuery reads everything the first round trip can carry.
//
// The head commit is pinned by oid rather than taken from commits(last: 1), so
// that the date compared against a thread's last reply belongs to the same
// commit the freshness check is looking at.
//
// reviews are asked for from the end, and paginated backwards from there: they
// arrive oldest first, and a window holding the oldest of them — CI and bot
// reviews add up — would say nothing about where anybody now stands. The size
// asked for is what the limit still has room for, so that what is written is
// what was asked for rather than a first page that outruns it.
const bodyQuery = `
query($owner: String!, $name: String!, $number: Int!, $headOid: GitObjectID!, $reviews: Int!) {
  viewer { login }
  repository(owner: $owner, name: $name) {
    headCommit: object(oid: $headOid) { ... on Commit { committedDate } }
    pullRequest(number: $number) {
      comments(first: 100) {
        totalCount
        pageInfo { hasNextPage endCursor }
        nodes { author { login __typename } body createdAt lastEditedAt url }
      }
      reviews(last: $reviews) {
        totalCount
        pageInfo { hasPreviousPage startCursor }
        nodes { author { login __typename } state body url submittedAt lastEditedAt }
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
        nodes { author { login __typename } body createdAt lastEditedAt url }
      }
    }
  }
}`

// reviewsPageQuery walks backwards from where the first window began, so that
// each page is older than the last and the whole stays oldest first once the
// pages are put back together.
const reviewsPageQuery = `
query($owner: String!, $name: String!, $number: Int!, $reviews: Int!, $cursor: String!) {
  repository(owner: $owner, name: $name) {
    pullRequest(number: $number) {
      reviews(last: $reviews, before: $cursor) {
        pageInfo { hasPreviousPage startCursor }
        nodes { author { login __typename } state body url submittedAt lastEditedAt }
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

// liveThreadQuery re-reads one thread as it is now, which is what a write
// checks itself against before it is sent.
const liveThreadQuery = `
query($threadId: ID!) {
  node(id: $threadId) {
    ... on PullRequestReviewThread {
      isResolved
      ` + newestComment + `
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
        nodes { author { login __typename } body createdAt url }
      }
    }
  }
}`
