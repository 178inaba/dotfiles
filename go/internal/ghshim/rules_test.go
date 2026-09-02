package ghshim

import "testing"

// TestEveryBodyFlagOffersARecoveryItHas holds the pairing the recovery field
// exists for. The two are not derivable from one another — a verb with no file
// form need not have a gh <noun> comment to send the body to instead — so
// nothing but this stops a row from offering a way out it does not have.
func TestEveryBodyFlagOffersARecoveryItHas(t *testing.T) {
	t.Parallel()

	for c, bf := range bodyFlagTable {
		t.Run(c.noun+" "+c.verb, func(t *testing.T) {
			t.Parallel()

			if bf.inlineLong == "" || bf.inlineShort == "" {
				t.Fatalf("%+v has no inline body flag, so rule 2 can never fire", bf)
			}
			switch bf.recovery {
			case recoverByFile:
				if bf.fileLong == "" || bf.fileShort == "" {
					t.Errorf("%+v offers a file form it does not have", bf)
				}
			case recoverByComment:
				if bf.fileLong != "" {
					t.Errorf("%+v sends the body to a separate comment although it has --%s", bf, bf.fileLong)
				}
				if !writes(command{noun: c.noun, verb: "comment"}) {
					t.Errorf("gh %s comment is not a command, so the recovery cannot be followed", c.noun)
				}
			}
		})
	}
}

// TestEveryBodyFlagTakesAValue is the drift the shell suite watched from the
// other side: the argv walk records a body only for a spelling the value-flag
// table also lists, so a half-updated pair would skip that verb's body scan
// without a word. TestDecideEveryBodyFlagIsScanned covers the same ground
// through Decide; this says which table is wrong when it fails.
func TestEveryBodyFlagTakesAValue(t *testing.T) {
	t.Parallel()

	for c, bf := range bodyFlagTable {
		t.Run(c.noun+" "+c.verb, func(t *testing.T) {
			t.Parallel()

			vf := valueFlagsFor(c)
			for _, long := range []string{bf.inlineLong, bf.fileLong} {
				if long != "" && !vf.long[long] {
					t.Errorf("--%s carries a body but is not registered as taking a value", long)
				}
			}
			for _, short := range []string{bf.inlineShort, bf.fileShort} {
				if short != "" && !containsAll(vf.short, short) {
					t.Errorf("-%s carries a body but is not registered as taking a value", short)
				}
			}
		})
	}
}

func containsAll(set, want string) bool {
	for i := 0; i < len(want); i++ {
		found := false
		for j := 0; j < len(set); j++ {
			if set[j] == want[i] {
				found = true
				break
			}
		}
		if !found {
			return false
		}
	}
	return true
}
