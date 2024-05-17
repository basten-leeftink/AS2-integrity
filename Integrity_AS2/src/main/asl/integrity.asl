agent(tom).

principle(tom, p1, 0.4).
principle(tom, p2, 0.2).
principle(tom, p3, 0.9).

intention(tom, p1, 0.2).
intention(tom, p2, 0.2).
intention(tom, p3, 0.8).

weight(tom, p1, 0.7).
weight(tom, p2, 0.8).
weight(tom, p3, 0.4).

sum(0).

!init(tom).

+!init(Agent) =>
    +sum(0);
    for (X in principle(Agent, X, P)) {
        !sub(Agent,X);
    }.

+!sub(Agent, X) :
    principle(Agent, X, P) &&
    intention(Agent, X, I) &&
    weight(Agent, X, W) &&
    sum(CurrentSum) &&
    D is (((P-I)*W)**2) &&
    NewSum is CurrentSum + D &&
    RootOfSum is NewSum ** 0.5=>

    -sum(CurrentSum);
    +sum(NewSum);
    #println(NewSum).