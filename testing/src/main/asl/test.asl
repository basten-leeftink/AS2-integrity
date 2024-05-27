// Initialize agents
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

// Initializing variables 
sum(0.0).
weightSum(0.0).
threshold(0.8).

// Initialize program 
!getnames().

// Looping through all the agents
+!getnames() =>
    for (Name in agent(Name)) {
        !init(Name);
    }.

// Looping through principles 
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

    // Updating sum in belief-base
    -sum(CurrentSum);
    +sum(NewSum);

// Calculate sum of weights 
+!distanceWeight(Agent, W):
    weightSum(CurrentSumWeight) &&
    NewSumWeight is CurrentSumWeight + W =>

    // Updating sum in belief-base
    -weightSum(CurrentSumWeight);
    +weightSum(NewSumWeight).

// Calculate normalized distance 
+!normalizedDistance(Agent) :
    sum(Dw) &&
    threshold(T) &&
    weightSum(Dmax) => 

    // Taking square root and invert distance 
    Alpha = (1 - (#nl.uva.sqrt.RootCalculator.calculateRoot(Dw,2) / #nl.uva.sqrt.RootCalculator.calculateRoot(Dmax,2)));

    if (Alpha > T) {
        #println("The perceived integrity of " + Agent + " is: " + Alpha + ". And is thus integer.");
    } else {
        #println("The perceived integrity of " + Agent + " is: " + Alpha + ". And is thus not integer.");
    };

    // Resetting belief base
    -sum(X);
    -weightSum(Dmax);
    +sum(0.0);
    +weightSum(0.0).