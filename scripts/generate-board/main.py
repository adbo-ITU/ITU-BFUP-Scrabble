import string
import pandas as pd
from dotenv import load_dotenv
import os

SIZE = 23
CENTER = SIZE // 2


def main():
    load_dotenv()

    sheet_id = os.getenv('SHEET_ID')

    if sheet_id is None:
        print(f'Environment variable SHEET_ID was not found.')
        exit(1)

    # NOTE: Requires that the sheet has enabled link sharing.
    url = f'https://docs.google.com/spreadsheets/d/{sheet_id}/export'

    board = [[None] * SIZE for _ in range(SIZE)]

    sheet_name = "Generate"
    try:
        data = pd.read_excel(url, sheet_name)
        data = data.where(pd.notnull(data), None)
        print(f"Loaded worksheet '{sheet_name}' from {url}")
    except:
        print(f"Failed to get data for worksheet '{sheet_name}'")
        exit(1)

    for row_index, row in enumerate(data.to_numpy()):
        for col_index, cell in enumerate(row):
            if cell is not None:
                print(f"cell: {cell}")
                board[row_index + 1][col_index] = cell.upper()

    print()
    draw_board(board)

    print()
    show_board_seed(board)


def draw_board(board):
    lines = '\n'.join([f'{r - CENTER:>3} # ' + ' '.join((col if col is not None else '·')
                                                        for col in row) + ' #' for r, row in enumerate(board)])

    col_nums = [list(f'{i - CENTER:>3}') for i in range(SIZE)]
    col_num_rows = [' '.join(r)
                    for r in list(zip(*[list(c) for c in col_nums]))]

    print('      ' + col_num_rows[0])
    print('      ' + col_num_rows[1])
    print('      ' + col_num_rows[2])

    print('    ' + (SIZE * 2 + 3) * '#')
    print(lines)
    print('    ' + (SIZE * 2 + 3) * '#')


def show_board_seed(board):
    tiles = []

    for r, row in enumerate(board):
        for c, letter in enumerate(row):
            if letter is None:
                continue

            char = letter.upper()
            id = string.ascii_uppercase.index(char) + 1
            points = 1
            coordinate = (c - CENTER, r - CENTER)
            tiles.append(f"({coordinate}, ({id}u, ('{char}', {points})))")

    print('[ ' + '; '.join(tiles) + ' ]')


if __name__ == '__main__':
    main()
