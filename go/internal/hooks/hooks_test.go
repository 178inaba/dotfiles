package hooks

import (
	"encoding/json/v2"
	"testing"

	"github.com/google/go-cmp/cmp"
)

func TestParse(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		in   string
		want Payload
	}{
		{
			name: "every field the nine hooks read",
			in: `{"session_id":"s-1","agent_id":"a-1","tool_name":"Edit","cwd":"/w",
			      "message":"hello","notification_type":"idle_prompt",
			      "tool_input":{"command":"ls","file_path":"/w/SKILL.md"}}`,
			want: Payload{
				SessionID: "s-1", AgentID: "a-1", ToolName: "Edit", Dir: "/w",
				Message: "hello", NotificationType: "idle_prompt",
				Command: "ls", FilePath: "/w/SKILL.md",
			},
		},
		{
			// NotebookEdit carries the target under a different key, and the
			// two hooks that read a path treat them as one field.
			name: "notebook_path stands in for file_path",
			in:   `{"tool_name":"NotebookEdit","tool_input":{"notebook_path":"/w/n.ipynb"}}`,
			want: Payload{SessionID: unknownSession, ToolName: "NotebookEdit", FilePath: "/w/n.ipynb"},
		},
		{
			name: "file_path wins when both are present",
			in:   `{"tool_input":{"file_path":"/w/a","notebook_path":"/w/b"}}`,
			want: Payload{SessionID: unknownSession, FilePath: "/w/a"},
		},
		{
			name: "a payload with no session names the unknown one",
			in:   `{"tool_name":"Bash"}`,
			want: Payload{SessionID: unknownSession, ToolName: "Bash"},
		},
		{
			// Both ids become path components, so everything that could leave
			// the state directory is stripped rather than escaped.
			name: "the ids keep only the characters a file name may hold",
			in:   `{"session_id":"../../etc/passwd","agent_id":"a b/../c"}`,
			want: Payload{SessionID: "etcpasswd", AgentID: "abc"},
		},
		{
			name: "an id of nothing but stripped characters is no id at all",
			in:   `{"session_id":"///","agent_id":"..."}`,
			want: Payload{SessionID: unknownSession},
		},
		{
			name: "malformed input reads as an empty payload",
			in:   `not json at all`,
			want: Payload{SessionID: unknownSession},
		},
		{
			name: "empty input reads as an empty payload",
			in:   ``,
			want: Payload{SessionID: unknownSession},
		},
		{
			// encoding/json/v2 rejects these where jq accepted them. The
			// result is the same fail-open payload every hook already handles.
			name: "duplicate keys read as an empty payload",
			in:   `{"tool_name":"Bash","tool_name":"Edit"}`,
			want: Payload{SessionID: unknownSession},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			if diff := cmp.Diff(tt.want, Parse([]byte(tt.in))); diff != "" {
				t.Errorf("Parse() mismatch (-want +got):\n%s", diff)
			}
		})
	}
}

func TestDirectiveMarshalsOnlyWhatIsSet(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name string
		in   Directive
		want string
	}{
		{name: "empty", in: Directive{}, want: `{}`},
		{name: "bell", in: Directive{TerminalSequence: "\a"}, want: `{"terminalSequence":"\u0007"}`},
		{name: "message", in: Directive{SystemMessage: "boom"}, want: `{"systemMessage":"boom"}`},
		{
			name: "both",
			in:   Directive{TerminalSequence: "\a", SystemMessage: "boom"},
			want: `{"terminalSequence":"\u0007","systemMessage":"boom"}`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			got, err := json.Marshal(tt.in)
			if err != nil {
				t.Fatalf("Marshal: %v", err)
			}
			if string(got) != tt.want {
				t.Errorf("Marshal() = %s, want %s", got, tt.want)
			}
		})
	}
}

func TestDirectiveIsEmpty(t *testing.T) {
	t.Parallel()

	if !(Directive{}).IsEmpty() {
		t.Error("the zero Directive reports itself as worth writing")
	}
	if (Directive{SystemMessage: "boom"}).IsEmpty() {
		t.Error("a Directive carrying a message reports itself as empty")
	}
}
