#pragma once
#pragma once

#include <iostream>
#include <cstring>
#include <string>
#include <vector>
#include <map>
using namespace std;

class Sudoku
{
public:
    Sudoku()
    {
        case1();
    }
    void case1()
    {
        
        vector<vector<char>> board = {
            {'5', '3', '.', '.', '7', '.', '.', '.', '.'},
            {'6', '.', '.', '1', '9', '5', '.', '.', '.'},
            {'.', '9', '8', '.', '.', '.', '.', '6', '.'},
            {'8', '.', '.', '.', '6', '.', '.', '.', '3'},
            {'4', '.', '.', '8', '.', '3', '.', '.', '1'},
            {'7', '.', '.', '.', '2', '.', '.', '.', '6'},
            {'.', '6', '.', '.', '.', '.', '2', '8', '.'},
            {'.', '.', '.', '4', '1', '9', '.', '.', '5'},
            {'.', '.', '.', '.', '8', '.', '.', '7', '9'}
        };
        //update(board);
        printSudoko(board);
        solveSudoku(board);
        cout << "\nsolved puzzle";
        printSudoko(board);
    }
    void update(vector<vector<char>>& board)
    {
        
        for (int r = 0; r < board.size(); r++) {
            
            for (int c = 0; c < board.size(); c++) {
                board[r][c] = 'a';
                
            }
        }
    }
    void printSudoko(vector<vector<char>>& board)
    {
        cout << "\n";
        for (int r = 0; r < board.size(); r++) {
            cout << "\n";
            for (int c = 0; c < board.size(); c++) {
                int val = board[r][c] - '0';
                cout << val << " | ";
            }
        }
    }

    bool validValue(vector<vector<char>>& board,int r, int c, char v)
    {
        for (int i = 0; i < 9; i++)
        {
            if (board[r][i] == v) // is number already there on that row
                return false;
        }
        for (int i = 0; i < 9; i++)
        {
            if (board[i][c] == v) // is number already thre on that column
                return false;
        }
        int r0 = r - r % 3; 
        int c0 = c - c % 3;
        for (int i = 0; i < 3; i++)
        {
            for (int j = 0; j < 3; j++) {
                if (board[r0 + i][c0 + j] == v) {
                    return false;
                }
            }
        }
        //cout << "\npossible r=" << r << " c=" << c << " v=" << v;
        return true;
    }

    bool validSolution(vector<vector<char>>& board)
    {
        for (int r = 0; r < board.size(); r++) {
            for (int c = 0; c < board.size(); c++) {
                if (board[r][c] == '.')
                    return false;
            }
        }
        return true;

    }
    void solveSudoku(vector<vector<char>>& board) {
    
        for (int r = 0; r < board.size();r++) {
            //cout << "\n";
            for (int c = 0; c < board.size(); c++) {
                
                if (board[r][c] == '.') { // empty position
                    //val = 0;
                    for (int possible_val = 0; possible_val < 9; possible_val++) {
                        if (validValue(board, r, c, '1' + possible_val)) {
                            board[r][c] = '1' + possible_val;
                            solveSudoku(board);
                            if (validSolution(board) == true){
                                return;
                            }
                            else {
                                // not a valid solution reset the value back to original and try again
                                board[r][c] = '.'; // make the position empty again
                            }
                        }
                    }
                    return;
                  
                }
                //cout << board[r][c] << " | ";
                
            }
        }
        //cout << "start solved";
        //printSudoko(board);
        //cout << "end solved";
        return;
    }
};



