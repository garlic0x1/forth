(asdf:defsystem "forth"
  :author "garlic0x1"
  :license "MIT"
  :description "An experimental Forth interpreter."
  :depends-on (:alexandria :str :named-readtables)
  :components ((:file "forth")))
