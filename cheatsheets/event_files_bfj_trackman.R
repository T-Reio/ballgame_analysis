pitch_call <- c(
  "StrikeCalled", "StrikeSwinging", "BallCalled",
  "BallinDirt", "BallIntentional", "HitByPitch", "FoulBall", "InPlay"
)

play_results <- c(
  "Single", "Double", "Triple", "HomeRun", "Error", 
  "FieldersChoice", "Out", "Sacrifice", "Undefined"
)

swing <- pitch_call[c(2, 7, 8)]
called <- pitch_call[c(1, 3)]
contact <- pitch_call[c(7, 8)]


base_hit <- play_results[1:4]
