Attribute VB_Name = "Standard_binomial_tree_model1"
Option Explicit
Sub Standard_binomial_tree_model()

'Code for Standard binomial options pricing model for call option.
'Does not include early exercise

    Dim d, r, S, K, sigma, up, down, p, backward_price, exercise_price, M, e, dct_f As Double
    Dim i, j As Integer
    Dim stock_price_array As Variant
    Dim payoff_array As Variant
    
'===============================================================================================================
'Input:

    d = 6 / 100 'Dividend yield
    r = 1.302 / 100 'Risk-free rate
    S = 26.9 'Current stock price
    K = 25 'Strike price
    sigma = 45 / 100 'Volatility
    M = 2.8 'Early exercise ratio
    e = 9.5 / 100 'Employee exit rate
    Const step_size As Double = 6 / 200 '# of steps in 1 year
    Const t As Double = 6 '# of years
    Const v As Double = 1 'Remaining vesting period
'===============================================================================================================
    
    Worksheets("Stock Price Process").UsedRange.ClearContents
    Worksheets("Payoff Backward Induction").UsedRange.ClearContents
    
    'CRR model
    up = Exp(sigma * Sqr(step_size))
    down = 1 / up
    p = (Exp((r - d) * step_size) - down) / (up - down)
    dct_f = Exp(-1 * r * step_size) 'Discounting factor
    
    'Modelling stock price process
    ReDim stock_price_array(1 To Int(t / step_size + 1), 1 To Int(t / step_size + 1))
    ReDim payoff_array(1 To Int(t / step_size + 1), 1 To Int(t / step_size + 1))
    
    stock_price_array(1, 1) = S
    Worksheets("Stock Price Process").Cells(1, 1) = stock_price_array(1, 1)
    For j = 2 To Int(t / step_size + 1)
        For i = 1 To j
            If i = 1 Then
                stock_price_array(i, j) = stock_price_array(i, j - 1) * up
            Else
                stock_price_array(i, j) = stock_price_array(i - 1, j - 1) * down
            End If
            Worksheets("Stock Price Process").Cells(i, j) = stock_price_array(i, j)
        Next i
    Next j
    
    'Calculate Terminal Payoff and Perform Backward Induction
    For j = Int(t / step_size + 1) To 1 Step -1
        For i = 1 To j Step 1
            If j = Int(t / step_size + 1) Then
                payoff_array(i, j) = WorksheetFunction.Max(stock_price_array(i, j) - K, 0)
            Else
                backward_price = dct_f * (p * payoff_array(i, j + 1) + (1 - p) * payoff_array(i + 1, j + 1))
                exercise_price = stock_price_array(i, j) - K
                If (j - 1) * step_size > v Then
                    If stock_price_array(i, j) >= K * M Then
                        payoff_array(i, j) = exercise_price
                    Else
                        payoff_array(i, j) = (1 - e * step_size) * backward_price + e * step_size * WorksheetFunction.Max(exercise_price, 0)
                    End If
                Else
                    payoff_array(i, j) = backward_price '(1 - e * step_size) * backward_price
                End If
            End If
            Worksheets("Payoff Backward Induction").Cells(i, j) = payoff_array(i, j)
        Next i
    Next j
    
    
    
    
End Sub
