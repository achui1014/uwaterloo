def place_stone(board, stone, row, col):
    '''
    Returns an updated board with stone placed in the (row, col) position on the board.

    place_stone: Board (anyof 'B' 'W') Nat Nat -> Board
    Requires:
        * board must not be in the 'Win' or 'Draw' state
        * 0 <= row and col < 9
    '''
    updated = [row[:] for row in board]
    for row_num in range(len(updated)):
        if row_num == row:
            for col_num in range(len(updated[row_num])):
                if col_num == col:
                    if updated[row_num][col_num] == '':
                        updated[row_num][col_num] = stone
    return updated

board = [[ '',  '',  '',  '',  '',  '',  '',  '',  ''],
         [ '',  '',  '',  '',  '',  '',  '',  '',  ''],
         [ '',  '',  '',  '',  '',  '',  '',  '',  ''],
         [ '',  '',  '',  '',  '',  '',  '',  '',  ''],
         [ '',  '',  '',  '',  '',  '',  '',  '',  ''],
         [ '', 'W',  '',  '',  '',  '',  '',  '',  ''],
         [ '', 'W',  '',  '',  '',  '',  '',  '',  ''],
         [ '', 'W',  '',  '',  '',  '',  '',  '',  ''],
         [ '', 'W', 'B', 'B', 'B', 'B',  '',  '',  '']]