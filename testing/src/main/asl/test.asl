agent(tom).
agent(paula).

// Belief-base Paula 
principle(paula, p1, 0).
principle(paula, p2, 0.2).
principle(paula, p3, 0).
principle(paula, p4, 0).
principle(paula, p5, 0.2).

intention(paula, p1, 0.5).
intention(paula, p2, 0.6).
intention(paula, p3, 0.7).
intention(paula, p4, 0.6).
intention(paula, p5, 0.5).


// Belief-base Tom
principle(tom, p1, 0.8).
principle(tom, p2, 0.2).
principle(tom, p3, 0.6).
principle(tom, p4, 0.4).
principle(tom, p5, 0.2).

intention(tom, p1, 0.6).
intention(tom, p2, 0.1).
intention(tom, p3, 0.4).
intention(tom, p4, 0.2).
intention(tom, p5, 0.2).

// How important are the principles?
weight(p1, 0.7).
weight(p2, 0.8).
weight(p3, 0.4).
weight(p4, 0.4).
weight(p5, 0.4).


sum(0).
weightSum(0).
threshold(0.8).

!getnames().

// Loop through all the agents
+!getnames() =>
    for (Name in agent(Name)) {
        !init(Name);
    }.

// Loop through principles 
+!init(Agent) =>
    for (X in principle(Agent, X, P)) {
        !distanceSum(Agent,X);
    };
    for (W in weight(X, W)) {
        !distanceWeight(Agent,W);
    };
    !normalizedDistance(Agent).

// Calulate weighted sum of distance 
+!distanceSum(Agent, X) :
    principle(Agent, X, P) &&
    intention(Agent, X, I) &&
    weight(X, W) &&
    sum(CurrentSum) &&
    D is W*(((P-I))**2) &&
    NewSum is CurrentSum + D =>

    -sum(CurrentSum);
    +sum(NewSum).

// Calculate sum of weights 
+!distanceWeight(Agent, W):
    weightSum(CurrentSumWeight) &&
    NewSumWeight is CurrentSumWeight + W =>

    -weightSum(CurrentSumWeight);
    +weightSum(NewSumWeight).

// Calculate normalized distance 
+!normalizedDistance(Agent) :
    sum(Xtest) &&
    threshold(T) &&
    weightSum(Ytest) => 
    if (1 -(Xtest / Ytest) > T) {
        #println("The perceived integrity of " + Agent + " is: " + (1 -(Xtest / Ytest))+ ". And is thus integer.");
    } else {
        #println("The perceived integrity of " + Agent + " is: " + (1 -(Xtest / Ytest))+ ". And is thus not integer.");
    };
    -sum(X);
    -weightSum(Ytest);
    +sum(0);
    +weightSum(0).