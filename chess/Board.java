public class Board {
  private final Square[][] board = new Square[8][8];
  private char whitePawn = (char) 9817;
  private char blackPawn = (char) 9823;

 public Board(char whiteGap, char blackGap) {
   for (int i = 0; i < 8; i++) {
    for (int j = 0; j < 8; j++) {
        Square sq = new Square(i, j);
        board[i][j] = sq;  
        sq.setOccupier(NONE); 
    }
   } 
  
   for (int i = 0; i < 8; i++) {
         if (i == whiteGap - 'a') {
            board[6][i].setOccupier(NONE);
         } else {
            board[6][i].setOccupier(WHITE);         
         }
   } 
   
   for (int j = 0; j < 8; j++) {
       if (j == blackGap - 'a') {
           board[2][j].setOccupier(NONE);
       } else {
           board[2][j].setOccupier(BLACK);         
       }
   }
    
 }

 public Square getSquare(int x, int y) {

 }

 public void applyMove(Move move) {

 }

 public void unapplyMove(Move move) {

 }

 public void display() {

 } 


}
