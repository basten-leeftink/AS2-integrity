i(5, 8).
p(10, 8).
w(0.1, 0.9).


distance(A,I1,I2,P1,P2,W1,W2):- DA is P1 - I1 && DB is P2 - I2 && WQDA is W1 * (DA**2) && WQDB is W2 * (DB * DB) && WQDS is WQDA + WQDB && WQRDS is WQDS ** 0.5 && WQRNDS is WQRDS / (W1 + W2) && A is 1 - WQRNDS .

!init(I1,I2,P1,P2,W1,W2).

+!init(I1,I2,P1,P2,W1,W2) : i(I1,I2) && p(P1,P2) && w(W1,W2) && distance(A,I1,I2,P1,P2,W1,W2) =>
    #println(A).

