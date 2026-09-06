package ghapi

import (
	"errors"

	"github.com/178inaba/dotfiles/go/internal/ghmd"
)

// The gh shim refuses a body that numbers its items with bare #N, because
// GitHub autolinks those and notifies unrelated issues; a notification cannot
// be taken back. This module writes GitHub in process, where the shim never
// sees the body, so the same judgement has to be made here.
//
// Calling it from each write path would leave the next path free to forget,
// which is how the gap it closes was opened. So it is not a call any path
// makes: a body reaches a writer only as a Body, and the one way to make one
// with text in it runs the judgement.

// Body is a markdown body GitHub will render, judged fit to send.
//
// The zero value is the empty body, which every judgement passes, so nothing
// is lost by it being constructible: what must not be constructible is a Body
// holding text nobody judged, and NewBody is the only way to get one.
type Body struct{ text string }

// NewBody judges text and answers with the body to send, or with what is
// wrong with it.
//
// The error says what was found and what to write instead, in the words the gh
// shim uses for the same body; where the text came from is the caller's to
// add, since only the caller knows whether it is a file, a field or a flag.
func NewBody(text string) (Body, error) {
	if distinct := ghmd.BareHashRefs(text); distinct >= ghmd.BareHashRefLimit {
		return Body{}, errors.New(ghmd.BareHashRefsRefusal(distinct))
	}
	return Body{text: text}, nil
}

// String is the text to send.
func (b Body) String() string { return b.text }

// Substitute fills in the #{NAME} placeholders numbers has a number for, and
// answers with the ones it could not fill.
//
// No second judgement is made about what comes out, and the result is a Body
// for that reason rather than in spite of it. What the bare-#N rule guards
// against is item numbering somebody typed; a number put in here names an
// issue the caller declared — and a run against a new repository fills in 1, 2
// and 3, which a second judgement would refuse.
func (b Body) Substitute(numbers map[string]int) (Body, []ghmd.Placeholder) {
	text, left := ghmd.Substitute(b.text, numbers)
	return Body{text: text}, left
}
