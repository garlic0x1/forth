(asdf:defsystem "forth"
  :author "garlic0x1"
  :license "MIT"
  :description "An experimental Forth interpreter."
  :depends-on (:alexandria)
  :components ((:file "package")
               (:file "forth")
               (:file "std")))
