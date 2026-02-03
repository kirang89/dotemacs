#!/bin/bash
# Profile Emacs startup and show slowest packages

EMACS_DIR="$HOME/.emacs.d"

echo "Profiling Emacs startup..."
echo ""

# Time the startup and get benchmark-init results
emacs --batch -l "$EMACS_DIR/init.el" --eval "
(progn
  (require 'benchmark-init)
  (benchmark-init/activate)

  ;; Get total startup time
  (message \"=== TOTAL STARTUP TIME ===\")
  (message \"%.2f seconds\" (float-time (time-subtract after-init-time before-init-time)))
  (message \"\")

  ;; Get top packages by load time
  (message \"=== TOP 10 SLOWEST PACKAGES ===\")
  (let* ((entries (benchmark-init/node-children benchmark-init/durations-tree))
         (sorted (sort (copy-sequence entries)
                       (lambda (a b)
                         (> (benchmark-init/node-duration a)
                            (benchmark-init/node-duration b)))))
         (top-10 (seq-take sorted 10))
         (rank 1))
    (dolist (entry top-10)
      (message \"%2d. %-40s %7.0f ms\"
               rank
               (benchmark-init/node-name entry)
               (* 1000 (benchmark-init/node-duration entry)))
      (setq rank (1+ rank))))
  (message \"\")

  ;; Show require vs load breakdown
  (message \"=== BY TYPE ===\")
  (let ((require-time 0)
        (load-time 0))
    (dolist (entry (benchmark-init/node-children benchmark-init/durations-tree))
      (if (eq (benchmark-init/node-type entry) 'require)
          (setq require-time (+ require-time (benchmark-init/node-duration entry)))
        (setq load-time (+ load-time (benchmark-init/node-duration entry)))))
    (message \"require: %.0f ms\" (* 1000 require-time))
    (message \"load:    %.0f ms\" (* 1000 load-time)))
)" 2>&1 | grep -E "^(===|[0-9]|require:|load:|\.)"
