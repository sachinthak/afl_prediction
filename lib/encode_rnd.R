# encode round type (i.e. regular, qualifying, elimination, etc), since Stan cannot handle characters
encode_rnd <- function(rnd){
  if (rnd == 'Qualifying Final 1')
    -1
  else if (rnd == 'Qualifying Final 2')
    -2
  else if (rnd == 'Elimination Final 1')
    -3
  else if (rnd == 'Elimination Final 2')
    -4
  else if (rnd == 'Semi Final 1')
    -5
  else if (rnd == 'Semi Final 2')
    -6
  else if (rnd == 'Preliminary Final 1')
    -7
  else if (rnd == 'Preliminary Final 2')
    -8
  else if (rnd == 'Grand Final 1')
    -9
  else
    0
}
