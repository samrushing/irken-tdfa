;; -*- Mode: Irken -*-

(include "rx.scm")

(printf (rx-repr (parse-rx "[A-Z]+")) "\n")
(printf (pp-rx (parse-rx "(ab+)?")) "\n")
(printf (rx-repr (parse-rx "(ab+)?")) "\n")
(printf (pp-rx (parse-rx "a*bca*")) "\n")
(printf (rx-repr (parse-rx "a*bca*")) "\n")
(printf (rx-repr (parse-rx "([abc]~)?[de]+")) "\n")
(printf (rx-repr (parse-rx "b{a+}d")) "\n")
(printf (rx-repr (parse-rx "[a-z]\\[0")) "\n")
