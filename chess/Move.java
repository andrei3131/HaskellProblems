public class Move {
    private final Square from;
    private final Square to;
    private final capture;

public Move(Square from, Square to, boolean isCapture, boolean isEnPassantCapture) {
    this.from = from;
    this.to = to;
    this.capture = capture;
}

public Square getFrom() {
   return from;
}

public Square getTo() {
  return to;
}

//*******
public boolean isCapture() {
   
}

public boolean isEnPassantCapture() {

}

public String getSAN() {

}


//****

}
