from scipy import signal

def finding_peakspy(rowSums, PeakHeight, distance):
  
  indexes = signal.find_peaks(rowSums, height=PeakHeight, distance=distance)
  return indexes
  
