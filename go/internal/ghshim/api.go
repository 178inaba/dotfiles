package ghshim

import (
	"encoding/json/v2"
	"regexp"
	"strings"
)

// The fifth rule's reading of a gh api argv, and the predicates that decide
// what it is: which endpoint it addresses, whether it writes, and what GraphQL
// query it carries. body.go holds the same for the rules that read a body.

// apiScanned is what one walk of a gh api argv found.
type apiScanned struct {
	// endpoint is the first positional, as written; normaliseEndpoint reduces
	// it to the form the two patterns below are matched against.
	endpoint string
	// method is the value of -X/--method, empty when it was not given.
	method string
	// fields are the -F/--field and -f/--raw-field parameters, in order.
	fields []apiField
	// input is the value of --input. hasInput is what tells an empty one from
	// an absent one, which matters because the presence alone makes gh POST.
	input    string
	hasInput bool
}

// apiField is one key=value parameter. magic marks the -F/--field spelling,
// whose value gh reads from a file when it opens with @; -f/--raw-field takes
// its value as the string it is.
type apiField struct {
	key, value string
	magic      bool
}

// scanAPI reads the arguments that follow gh api.
func scanAPI(args []string) apiScanned {
	var s apiScanned

	walk(args, apiValueFlags, func(a arg) {
		if a.positional() {
			if s.endpoint == "" {
				s.endpoint = a.value
			}
			return
		}
		if !a.hasValue {
			return
		}
		switch {
		case a.is("method", "X"):
			s.method = a.value
		// --input has no short spelling, so there is none to pair it with.
		case a.is("input", ""):
			s.input, s.hasInput = a.value, true
		case a.is("field", "F"):
			s.fields = append(s.fields, newAPIField(a.value, true))
		case a.is("raw-field", "f"):
			s.fields = append(s.fields, newAPIField(a.value, false))
		}
	})
	return s
}

func newAPIField(value string, magic bool) apiField {
	key, v, _ := strings.Cut(value, "=")
	return apiField{key: key, value: v, magic: magic}
}

// isPOST reports whether the request will be sent with POST.
//
// gh switches from GET to POST as soon as the request carries parameters, so
// the absence of --method is not the absence of a write; and an explicit one
// beats that default, which is what --method GET is for.
func (s apiScanned) isPOST() bool {
	if s.method != "" {
		return strings.EqualFold(s.method, "POST")
	}
	return s.hasInput || len(s.fields) > 0
}

// normaliseEndpoint reduces the spellings of one endpoint to a single form.
//
// The order is the rule. A query string and a fragment go first, since either
// would otherwise hang off the end of a path the two patterns anchor. Then a
// URL becomes its path, since the slash to drop is the one the path starts
// with. api/v3/ is tried before api/, because dropping api/ from
// api/v3/repos/... would leave v3/repos/... and match nothing: GitHub
// Enterprise serves REST under /api/v3 and GraphQL under /api/graphql, while
// github.com serves both at the root of its api host.
//
// Cut rather than net/url, which the packages that read GitHub's own urls use:
// a parse here answers with an error as well, and there is no sound thing to
// do with one — passing the command on because its endpoint would not parse is
// the fail-open this rule exists to close, and refusing it would block reads
// that gh accepts.
func normaliseEndpoint(endpoint string) string {
	endpoint, _, _ = strings.Cut(endpoint, "#")
	endpoint, _, _ = strings.Cut(endpoint, "?")

	if _, rest, ok := strings.Cut(endpoint, "://"); ok {
		_, path, found := strings.Cut(rest, "/")
		if !found {
			return ""
		}
		endpoint = path
	}
	endpoint = strings.TrimPrefix(endpoint, "/")

	if after, ok := strings.CutPrefix(endpoint, "api/v3/"); ok {
		return after
	}
	return strings.TrimPrefix(endpoint, "api/")
}

// threadMutation matches either field name as a whole word, so that
// unresolveReviewThread, which the parent issue leaves out of scope, is not
// caught by the other half. RE2's \b is the ASCII boundary, and the class it
// is drawn around — [0-9A-Za-z_] — is what a GraphQL identifier is made of. A
// regexp rather than a parse: the query only has to be recognised, not read.
var threadMutation = regexp.MustCompile(
	`\b(?:addPullRequestReviewThreadReply|resolveReviewThread)\b`)

// replyEndpoint matches the REST endpoint that adds a reply to a review
// thread. A segment is anything without a slash, which is what admits gh's
// literal {owner} and {repo} placeholders alongside real names. The trailing
// slash is matched as well: GitHub answers 404 to it, but leaving a spelling
// out of a refusal that falls closed buys nothing.
var replyEndpoint = regexp.MustCompile(
	`^repos/[^/]+/[^/]+/pulls/[0-9]+/comments/[0-9]+/replies/?$`)

// threadMutationName is the mutation the query asks for, or empty for one that
// asks for neither. The name is returned rather than a bool so that the
// refusal can say which of the two it found.
func threadMutationName(query string) string {
	return threadMutation.FindString(query)
}

// queryText is the GraphQL query the request carries, gathered from every
// place gh would take one from, and the first file it could not read.
//
// A named file that cannot be read is reported rather than passed over, the
// same way the body rules treat theirs: gh exits before touching the API when
// it cannot read one, so refusing loses no command that would have succeeded.
// The two stdin spellings are the carve-out — reading standard input here
// would consume what gh is about to read. An --input that is not JSON, or that
// holds no query, contributes nothing and is left to gh.
func (s apiScanned) queryText() (text, source, reason string) {
	var b strings.Builder
	note := func(src, why string) {
		if source == "" {
			source, reason = src, why
		}
	}

	for _, f := range s.fields {
		if f.key != "query" {
			continue
		}
		path, isFile := strings.CutPrefix(f.value, "@")
		if !f.magic || !isFile {
			b.WriteString(f.value)
			b.WriteByte('\n')
			continue
		}
		if path == "-" {
			continue
		}
		content, why := readNamedFile(path)
		if why != "" {
			note("-F query=@"+path, why)
			continue
		}
		b.WriteString(content)
		b.WriteByte('\n')
	}

	if s.hasInput && s.input != "-" {
		content, why := readNamedFile(s.input)
		switch {
		case why != "":
			note("--input "+s.input, why)
		default:
			var request struct {
				Query string `json:"query"`
			}
			if err := json.Unmarshal([]byte(content), &request); err == nil {
				b.WriteString(request.Query)
			}
		}
	}
	return b.String(), source, reason
}
