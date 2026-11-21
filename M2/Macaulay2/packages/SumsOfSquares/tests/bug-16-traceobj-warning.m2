-- BUGS.md 16: TraceObj => true silently replaces a given objective.
-- Decided (LOG.md 1.7 item 5): a warning when both are given; behaviour
-- unchanged (pinned in tests/lead-semantics.m2).
R = QQ[x][t];
(err, out) = capture "solveSOS(x^2 + t*x + 1, t, RoundTol => 12, TraceObj => true)";
assert(not err and match("(?i)warning", out) and match("TraceObj", out))
-- no warning without an objective
(err, out) = capture "solveSOS(x^2 + t*x + 1, RoundTol => 12, TraceObj => true)";
assert(not err and not match("(?i)warning.*TraceObj", out))
