Il file grafici.R è quello in cui abbiamo generato i plot della prima presentazione.
I file .stan sono quelli usati per le simulazioni:
-recurrent.stan per la Gamma prior
-recurrent_bida.stan per Gamma prior Exp hyperprior
-recurrent_AR.stan per l'Autoregressive prior
I file modello48.R, modello48_bida.R, ... sono quelli contenenti i parametri da passare a Stan.
Nel file generation_of_models.R sono stati generate i modelli.
In model_choice.R sono contenuti gli indici WAIC per la scelta della prior.
In generation of models abbiamo compilato via Stan tutti i nostri modelli.
In analysis_bida_AR_model abbiamo fatto analisi dei modelli che abbiamo ottenuto.