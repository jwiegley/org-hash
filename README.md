# `org-hash`

Functions to add and verify hashes on Org-mode notes, which allows them to be
made “read-only“ in a sense.

Here is the configuration I am using:
```elisp
(use-package org-hash
  :after org
  :bind (:map
         org-mode-map
         ("C-c C-x #"   . org-hash-update-or-confirm)
         ("C-c C-x C-#" . org-hash-update-or-confirm-all)))
```
