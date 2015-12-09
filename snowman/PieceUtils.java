public class PieceUtils {

  public static Piece charToPiece(char c) {
     switch (c) {
      case '^'  : return Piece.EMITTER_NORTH; 
      case '>'  : return Piece.EMITTER_EAST;
      case 'v'  : return Piece.EMITTER_SOUTH;
      case '<'  : return Piece.EMITTER_WEST;
      case '|'  : return Piece.LASER_VERTICAL;
      case '-'  : return Piece.LASER_HORIZONTAL;
      case '+'  : return Piece.LASER_CROSSED;
      case '/'  : return Piece.MIRROR_SW_NE;
      case '\\' : return Piece.MIRROR_NW_SE;
      case '#'  : return Piece.WALL;
      case 'o'  : return Piece.TARGET;
      case ' '  : return Piece.EMPTY;
      case '@'  : return Piece.SNOWMAN;
      
     } 
     return null;
  }

  public static Piece[][] charsToPieces(char[] description,
                                        int width, int height) {
      Piece[][] ps = new Piece[width][height];
      for (int j = 0; j < ps[0].length; j++) {
            for (int i = 0; i < ps.length; i++) {
                ps[i][height - j - 1] = charToPiece(description[(j * width) + i]);
            }
      }
     return ps;
  }

  public static boolean isEmitter(Piece p) {
    switch (p) {
      case EMITTER_NORTH:
      case EMITTER_EAST:
      case EMITTER_SOUTH:
      case EMITTER_WEST:
        return true;
    }

    return false;
  }

  public static Coordinate findEmitter(Piece[][] pieces) { 
       for (int i = 0; i < pieces.length; i++) {
          for (int j = 0; j < pieces[i].length; j++) {
               if (isEmitter(pieces[i][j])) {
                   Coordinate coordinate = new Coordinate(i,j);
                   return coordinate;
               } 
          } 
        }  
      return null;
  }

  public static Piece hideLaser(Piece p) {
    switch (p) {
      case LASER_VERTICAL:
      case LASER_HORIZONTAL:
      case LASER_CROSSED:
        return Piece.EMPTY;
    }
    return p;
  }

  public static Piece addLaser(Piece p, boolean isHorizontal) {
    if (p == Piece.EMPTY && isHorizontal) {
            return Piece.LASER_HORIZONTAL;
         } else if (p == Piece.EMPTY && !(isHorizontal)) {
            return Piece.LASER_VERTICAL;
         } else if (p == Piece.LASER_VERTICAL && isHorizontal) {
            return Piece.LASER_CROSSED;
         } else if (p == Piece.LASER_HORIZONTAL && !(isHorizontal)) {
            return Piece.LASER_CROSSED;
         } else {
            return p;
         }
          
  }

  public static Coordinate move(Piece p, Coordinate c, int xo, int yo) {
      if (p == Piece.EMITTER_NORTH) {
         Coordinate c1 = new Coordinate(c.getX(), c.getY() + 1);
             return c1;
      } else if (p == Piece.EMITTER_EAST) {
         Coordinate c2 = new Coordinate(c.getX() + 1, c.getY());
             return c2;
      } else if (p == Piece.EMITTER_SOUTH) {
         Coordinate c3 = new Coordinate(c.getX(), c.getY() - 1);
             return c3;
      } else if (p == Piece.EMITTER_WEST) {
         Coordinate c4 = new Coordinate(c.getX() - 1, c.getY());
             return c4;
      } else if (p == Piece.EMPTY || p == Piece.LASER_HORIZONTAL || p == Piece.LASER_VERTICAL ||
                 p == Piece.LASER_CROSSED) {
          Coordinate c5 = new Coordinate(c.getX() + xo, c.getY() + yo);
             return c5;
      } else if (p == Piece.MIRROR_SW_NE) {
             Coordinate c6 = new Coordinate(c.getX() + yo, c.getY() + xo);
             return c6;
      } else if (p == Piece.MIRROR_NW_SE) {
             Coordinate c7 = new Coordinate(c.getX() - yo, c.getY() - xo);
             return c7;
      } else {
             return c;
      }




  }


  public static Piece rotate(Piece p) {
    switch (p) {
      case EMITTER_NORTH:
        return Piece.EMITTER_EAST;
      case EMITTER_EAST:
        return Piece.EMITTER_SOUTH;
      case EMITTER_SOUTH:
        return Piece.EMITTER_WEST;
      case EMITTER_WEST:
        return Piece.EMITTER_NORTH;
      case MIRROR_SW_NE:
        return Piece.MIRROR_NW_SE;
      case MIRROR_NW_SE:
        return Piece.MIRROR_SW_NE;
    }
    return p;
  }

}
