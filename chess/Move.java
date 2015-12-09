public class Move {
    private final Square from;
    private final Square to;
    private final boolean captured;
    private final boolean enpassantcapture;
    

public Move(Square from, Square to, boolean isCapture, boolean isEnPassantCapture) {
    this.from = from;
    this.to = to;
    this.captured = isCapture;
    this.enpassantcapture = isEnPassant;
} 

public Square getFrom() {
   return from;
}

public Square getTo() {
  return to;
}


public boolean isCapture() {
    return captured;   
}

public boolean isEnPassantCapture() {
  return enpassantcapture;
}

public String getSAN() {
   int x = to.getX();
   int y = to.getY();
   return ((char) ('a' + x)) + (y + 1); 
}



}
