#pragma once

#include <iostream>
#include <cstring>
#include <string>
#include <vector>
#include <map>
#include <algorithm>
using namespace std;

class NQueens
{
public:
    NQueens()
    {
        case1();
        case2();
    }

    void case1()
    {
        auto solutions = solveNQueens(4);
        print_solutions(solutions);
    }

    void case2()
    {
        auto solutions = solveNQueens(1);
        print_solutions(solutions);
    }

    void print_board(vector<string>& board)
    {
        int n = board.size();
        cout << "\nboard---------------------------------------------";
        for (auto row : board) {

            cout << "\n";
            cout << "[" << row << "] ";


        }
        cout << "\nend-board";

    }

    void print_solutions(vector<vector<string>>& solutions) {
        cout << "\n[";
        int number_of_solutions = solutions.size();
        int i = 0;
        for (auto solution : solutions) {
            i++;
            cout << "[";
            int number_of_rows = solution.size();
            int j = 0;
            for (auto row : solution) {
                j++;
                cout << "\"" << row << "\"";
                if (j < number_of_rows)
                {
                    cout << ",";
                }
            }
            cout << "]";
            if (i < number_of_solutions)
            {
                cout << ",";
            }
        }
        cout << "]";
    }

    //learned that you do not need to check all valid position only i < col
    //as we are moving one Q on one col at a time
    //this improves performance
    bool isvalid(int row, int col, vector<string>& grid, int n)
    {
        // left check
        for (int i = 0; i < col; i++)
        {
            if (grid[row][i] == 'Q') return false;
        }
        // upper left check 
        for (int i = row, j = col; i >= 0 && j >= 0; i--, j--)
        {
            if (grid[i][j] == 'Q') return false;
        }
        // lower left check
        for (int i = row, j = col; i < n && j >= 0; i++, j--)
        {
            if (grid[i][j] == 'Q') return false;
        }
        return true;
    }
    void solve(vector<vector<string>> &solution, vector<string>& board,int c) {
        int n = board.size();
        if (c == n)
        {
            solution.push_back(board);
            return;
        }
        for (int row = 0; row < n; row++)
        {
            if (isvalid(row, c, board, n))
            {
                board[row][c] = 'Q';
                solve(solution, board,c + 1);
                //print_board(board);
                board[row][c] = '.';
            }
        }

     
           

        
    }
  

    vector<vector<string>> solveNQueens(int n) {
       
        vector<vector<string>> solutions;
        vector<string>board(n, string(n, '.'));
        
        solve(solutions,board,0);
        
        return solutions;

    }
};

//https://leetcode.com/problems/n-queens/solutions/2640273/c-n-queen-with-explanation/
class NQueensAS {
public:
    NQueensAS()
    {
        case1();
        case2();
    }

    void case1()
    {
        auto solutions = solveNQueens(4);
        print_solutions(solutions);
    }

    void case2()
    {
        auto solutions = solveNQueens(1);
        print_solutions(solutions);
    }

    void print_solutions(vector<vector<string>>& solutions) {
        cout << "\n[";
        int number_of_solutions = solutions.size();
        int i = 0;
        for (auto solution : solutions) {
            i++;
            cout << "[";
            int number_of_rows = solution.size();
            int j = 0;
            for (auto row : solution) {
                j++;
                cout << "\"" << row << "\"";
                if (j < number_of_rows)
                {
                    cout << ",";
                }
            }
            cout << "]";
            if (i < number_of_solutions)
            {
                cout << ",";
            }
        }
        cout << "]";
    }
    bool isvalid(int row, int col, vector<string>& grid, int n)
    {
        // left check
        for (int i = 0; i < col; i++)
        {
            if (grid[row][i] == 'Q') return false;
        }
        // upper left check 
        for (int i = row, j = col; i >= 0 && j >= 0; i--, j--)
        {
            if (grid[i][j] == 'Q') return false;
        }
        // lower left check
        for (int i = row, j = col; i < n && j >= 0; i++, j--)
        {
            if (grid[i][j] == 'Q') return false;
        }
        return true;
    }
    void dfs(int col, vector<vector<string>>& ans, vector<string>& grid, int n)
    {
        if (col == n)
        {
            ans.push_back(grid);
            return;
        }
        for (int row = 0; row < n; row++)
        {
            if (isvalid(row, col, grid, n))
            {
                grid[row][col] = 'Q';
                dfs(col + 1, ans, grid, n);
                grid[row][col] = '.';
            }
        }
    }
    vector<vector<string>> solveNQueens(int n) {
        vector<string>grid(n, string(n, '.'));
        vector<vector<string>>ans;
        int col = 0;
        dfs(col, ans, grid, n);
        return ans;
    }
};


class NQueensOnMyOwn {
public:
    NQueensOnMyOwn()
    {
        case1();
        case2();
    }

    void case1()
    {
        auto solutions = solveNQueens(4);
        print_solutions(solutions);
    }

    void case2()
    {
        auto solutions = solveNQueens(1);
        print_solutions(solutions);
    }

    void print_solutions(vector<vector<string>>& solutions) {
        cout << "\n[";
        int number_of_solutions = solutions.size();
        int i = 0;
        for (auto solution : solutions) {
            i++;
            cout << "[";
            int number_of_rows = solution.size();
            int j = 0;
            for (auto row : solution) {
                j++;
                cout << "\"" << row << "\"";
                if (j < number_of_rows)
                {
                    cout << ",";
                }
            }
            cout << "]";
            if (i < number_of_solutions)
            {
                cout << ",";
            }
        }
        cout << "]";
    }
    bool anyQonDiagonalAndNotColOrRowAttack(vector<string>& board, int rq, int cq)
    {
        int n = board.size();
        int start_r = max<int>(0, n - rq);
        int start_c = min<int>(n, n - cq);
        for (int i = 1; i < n; i++)
        {
            auto up = max<int>(0, rq - i);
            auto down = min<int>(rq + i, n - 1);
            auto left = max<int>(0, cq - i);
            auto right = min<int>(cq + i, n - 1);
            /*
            cout << "\n curr r " << rq << " c" << cq;
            cout << "\n up " << up <<  " left " << left;
            cout << "\n up " << up << right;
            cout << "\n down " << down << " left " <<  left;
            cout << "\n down " << down << " right " << right;
            */
            if ((i != cq && board[rq][i] == 'Q') || // no col attack
                (i != rq && board[i][cq] == 'Q'))   // no row attack
            {
                return true;
            }

            if (up > rq && left < cq &&
                down < rq && right > cq) {
                if (board[up][left] == 'Q' || board[up][right] == 'Q' ||
                    board[down][left] == 'Q' || board[down][right] == 'Q')
                    //cout << "\ndia under attack";
                    return true;
            }
        }
        //cout << "\nno dia attack";
        return false;
    }

    bool nQunderAttack(vector<string>& board)
    {
        int n = board.size();
        int qfound = 0;
        for (int r = 0; r < n; r++) {
            for (int c = 0; c < n; ++c) {
                if (board[r][c] == 'Q') {
                    if (anyQonDiagonalAndNotColOrRowAttack(board, r, c) == true) {
                        return true;
                    }
                    else {
                        qfound = qfound + 1; // q with no threat
                    }
                }
            }
        }
        if (qfound == n) {
            return false;
        }
        return true; // all n Q not under attack

    }

    void solve(vector<vector<string>>& solution, vector<string>& board, int r, map<int, bool>& colmap,
        map<int, bool>& leftmap,
        map<int, bool>& rightmap) {

        int n = board.size();
        if (r == n) {
            //cout << "\n***r out of bounds " << r;
            return;
        }

        for (int c = 0; c < n; c++)
        {

            if (colmap.find(c) != colmap.end() ||
                leftmap.find(r - c) != leftmap.end() ||
                rightmap.find(r + c) != rightmap.end())
            {
                //cout << "\n already tried r " << r << " c " << c;
                continue; // if every col is already tried then we have a solution
            }
            //cout << "\nfound new position r " << r << " c " << c;
            //cout << "\n " << c << "adding c";
            colmap.insert({ c,true });
            leftmap.insert({ r - c,true });
            rightmap.insert({ r + c,true });

            board[r][c] = 'Q';
            //print_board(board);
            solve(solution, board, r + 1, colmap, leftmap, rightmap);
            //print_board(board);
            if (nQunderAttack(board) == false)
            {
                //cout << "\n--------------------found solution -----------------";


                solution.push_back(board);


            }
            // see if there is one more solution
            board[r][c] = '.';
            colmap.erase(c);
            leftmap.erase(r - c);
            rightmap.erase(r + c);

        }



    }


    vector<vector<string>> solveNQueens(int n) {
        //vector<vector<char>> board;
        vector<vector<string>> solutions;
        //board.resize(n,vector<char>(n));

        /*for (int r = 0; r < n; r++)
        {
            for (int c = 0; c < n; ++c) {
                board[r][c] = '.';

            }
        }*/
        vector<string>board(n, string(n, '.'));
        //print_board(board);
        map<int, bool> colmap;
        map<int, bool> leftmap;
        map<int, bool> rightmap;

        solve(solutions, board, 0, colmap, leftmap, rightmap);
        //print_board(board);

        return solutions;

    }
};