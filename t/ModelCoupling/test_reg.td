; GPRM.yml
(seq
    '(* '6 '7)
    '(reg.write '1 '6)
    '(reg.write '2 (reg.read '1))
    '(reg.inc '2)
    '(* (reg.read '1) (reg.read '2))
)

