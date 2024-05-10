weights(0.1, 0.9).  
perceptionOfPrinciples(0.5, 0.6). 
perceptionOfIntentions(0.7, 0.8). 

integrity(X, W1, W2, D1, I1, D2, I2):- DiscrepancyD is D1 - D2 && DiscrepancyI is I1 - I2 && WD is W1 * (DiscrepancyD * DiscrepancyD) && WI is W2 * (DiscrepancyI * DiscrepancyI) && SumW is WD + WI && SumN is Sumw / (W1+W2) && X is 1 - SumW.  

!innit.

+!innit: perceptionOfIntentions(D1,I1) && perceptionOfPrinciples(D2, I2) && weights(W1, W2) =>
    #println("Starting integrity check.");
    perceptionOfIntentions(D1, I1);
    perceptionOfPrinciples(D2, I2);
    weights(W1,W2);
    integrity(X,W1,W2,D1,I1,D2,I2);
    #println(X).

