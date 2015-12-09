public class DoNotMeltTheSnowman {

  public static void main(String[] args) {
    int level = Integer.parseInt(args[0]);
    Level[] lev = Levels.getLevels();
    Level l = lev[level];
    char[] chars = l.getCharArray();
    Board board = new Board(PieceUtils.charsToPieces(chars, l.getWidth(), l.getHeight()));
    while (true) {
      Result res = board.fireLaser();
      board.renderBoard();
      if (res == Result.HIT_TARGET) {
        System.out.println("You won!!!");
        break;
      } else if (res == Result.MELT_SNOWMAN) {
        System.out.println("You lost");
        break;
      } else {
        System.out.println("introduce row and column of a piece");
        int x = IOUtil.readInt();
        int y = IOUtil.readInt();
        Coordinate c = new Coordinate(y, x);
        board.rotatePiece(c);       
        board.clearLasers();
      }
 

    }

   
    


}

}
