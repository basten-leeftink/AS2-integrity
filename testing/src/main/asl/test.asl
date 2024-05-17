agent(tom).

principle(tom, p1, 0.8).
principle(tom, p2, 0.7).
principle(tom, p3, 0.6).
principle(tom, p4, 0.4).
principle(tom, p5, 0.2).

intention(tom, p1, 0.4).
intention(tom, p2, 0.1).
intention(tom, p3, 0.4).
intention(tom, p4, 0.2).
intention(tom, p5, 0.2).

weight(tom, p1, 0.9).
weight(tom, p2, 0.8).
weight(tom, p3, 0.4).
weight(tom, p4, 0.4).
weight(tom, p5, 0.4).

sum(0).
weightSum(0).

!init(tom).

+!init(Agent) =>
    for (X in principle(Agent, X, P)) {
        !distanceSum(Agent,X);
    };

    for (W in weight(Agent, X, W)) {
        !distanceWeight(Agent,W);
    };
    
    !normalizedDistance(Agent).

+!distanceSum(Agent, X) :
    principle(Agent, X, P) &&
    intention(Agent, X, I) &&
    weight(Agent, X, W) &&
    sum(CurrentSum) &&
    D is W*(((P-I))**2) &&
    NewSum is CurrentSum + D =>

    -sum(CurrentSum);
    +sum(NewSum).


+!distanceWeight(Agent, W):
    weightSum(CurrentSumWeight) &&
    NewSumWeight is CurrentSumWeight + W =>

    -weightSum(CurrentSumWeight);
    +weightSum(NewSumWeight).

+!normalizedDistance(Agent) :
    sum(WeightedDistance) &&
    weightSum(DMax) => 
    #println("The perceived integrity of " + Agent + " is: " + (1 - (WeightedDistance / DMax))).
