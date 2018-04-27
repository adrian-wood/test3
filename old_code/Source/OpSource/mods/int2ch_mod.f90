MODULE int2ch_mod
  IMPLICIT NONE
  INTEGER :: I_
  CHARACTER (LEN=1), DIMENSION(0:255)::int2ch=(/(char(i_),i_=0,255)/)
END MODULE int2ch_mod
