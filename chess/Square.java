public class Square {
  private final int x;
  private final int y;
  private Colour colour;
 
  public Square(int x, int y) {
    this.x = x;
    this.y = y;
  }

  public int getX() {
     return x;
  }
   
  public int getY() {
     return y;
  }

  public Colour occupiedBy() {
     return colour;
  }
  
  public void setOccupier(Colour colour) {
     this.colour = colour;
  } 

}
