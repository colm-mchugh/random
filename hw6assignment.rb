# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  All_My_Pieces = All_Pieces +
                  [rotations([[0,0],[1,0],[0,1],[1,1],[2,0]]), # square plus one block
                   [[[0,0],[-2, 0], [-1,0],[1,0],[2,0]],      # loong piece
                    [[0,0],[0,-2],[0,-1],[0,1],[0,2]]],
                   rotations([[0,0],[1,0],[0,1]])]  # square minus one block

  # your enhancements here

  Cheat_Piece = [[[0, 0]]]
  
  def self.my_next_piece(board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.my_cheat_piece(board)
    MyPiece.new(Cheat_Piece, board)
  end
  
end

class MyBoard < Board
  # your enhancements here

  def initialize (game)
    super(game)
    @current_block = MyPiece.my_next_piece(self)
    @cheats = false
  end
  
  # rotates the current piece 180 degrees
  def flip_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, -2)
    end
    draw
  end

  def inc_cheats
    if @score >= 100 and !@cheats and !game_over? and @game.is_running?
      @cheats = true
      @score -= 100
      @game.update_score
    end
  end

  # gets the next piece
  def next_piece
    if @cheats
      @current_block = MyPiece.my_cheat_piece(self)
      @cheats = false
    else
      @current_block = MyPiece.my_next_piece(self)
    end
    @current_pos = nil
  end

  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.
  # this is altered from the Board class because pieces can have 3, 4 or 5 points
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    index = 0
    locations.each{|p|
      @grid[p[1]+displacement[1]][p[0]+displacement[0]] = @current_pos[index]
      index += 1
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
  
end

class MyTetris < Tetris
  # your enhancements here

  def key_bindings
    super
    @root.bind('u', proc {@board.flip_180})
    @root.bind('c', proc {@board.inc_cheats})
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  
end


