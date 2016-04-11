rm -rf "racr/racket-bin"
mkdir -p "racr/racket-bin/racr"
plt-r6rs ++path "/racr/racr/racket-bin" --install --collections "/racr/racr//racket-bin" "/racr/racr/core.scm"
plt-r6rs ++path "/racr/racr/racket-bin" --install --collections "/racr/racr//racket-bin" "/racr/racr/testing.scm"
