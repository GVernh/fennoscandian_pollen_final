sig_test = function(p) {
  if (p < 0.05) {
    sig_chi = "*"
  }
  
  if (p < 0.001) {
    sig_chi = "**"
  }
  
  if (p > 0.05) {
    sig_chi = ""
  }
  return(sig_chi)
}