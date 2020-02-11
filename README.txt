Il file grafici_times_ij.R è quello in cui abbiamo generato i plot della prima presentazione e su cui abbiamo costruito la matrice times (matrice che ci dice il tempo di ogni donazione per ciascun donatore)
I file .stan sono quelli usati per le simulazioni:
-recurrent.stan per la Gamma prior
-recurrent_bida.stan per Gamma prior Exp hyperprior
-recurrent_AR.stan per l'Autoregressive prior
-recurrent_spline.stan per il modello con la non constant baseline function
-recurrent_dp.stan per il modello con dp per prior for frailties 
I file modello48.R, modello48_bida.R, ... sono quelli contenenti i parametri da passare a Stan.
Nel file generation_of_models.R sono stati generate i modelli.
In model_choice.R sono contenuti gli indici WAIC per la scelta della prior.