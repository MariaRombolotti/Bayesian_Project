# Bayesian_Project
Il nostro progetto si basava sul predire il numero di donazioni in uno specifico centro donazioni.
I dati ci sono stati forniti dall'associazione AVIS.
In particolare il nostro compito era, a partire da un modello proposto di eventi ricorrenti, provare a modificare le lambda prior (in particolare abbiamo provato con delle semplici gamma, delle gamma con iperparametro esponenziale e una prior autoregressiva), provare a implementare le frialties con un processo di Dirichlet ed infine tentare di cambiare la baseline function da costante a tratti a lineare. Dopo aver implementato tutti questi modelli dovevamo scegliere il migliore.

Il file grafici_times_ij.R è quello dove abbiamo generato i plot della prima presentazione e su cui abbiamo costruito la matrice times (matrice che ci dice il tempo di ogni donazione per ciascun donatore)

I file .stan sono quelli usati per le simulazioni:

-recurrent.stan per la Gamma prior

-recurrent_bida.stan per Gamma prior Exp hyperprior

-recurrent_AR.stan per l'Autoregressive prior

-recurrent_spline1.stan per il modello con la non constant baseline function

-recurrent_DP.stan per il modello con dp per prior for frailties 

I file modello48.R, modello48_bida.R, ... sono quelli contenenti i parametri da passare a Stan.

Nel file generation_of_models.R sono stati generate i modelli.

In model_choice.R sono contenuti gli indici WAIC per la scelta della prior.
