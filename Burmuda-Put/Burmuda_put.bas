Option Explicit
Sub Exotic_put_binomial_tree_model()
'Exercise style: Burmuda
'Feature: Put

    Dim d, r, S, K, sigma, up, down, p, backward_price, exercise_price, dct_f, decision, v, t, step_size As Double
    Dim i, j As Integer
    Dim stock_price_array As Variant
    Dim payoff_array As Variant
    Dim ws_assp As Worksheet
    
    Set ws_assp = Worksheets("Input") 'Change according to worksheet
    
'===============================================================================================================
'Input: Change according to worksheet
    With ws_assp
        d = .Range("E18") 'Dividend yield
        r = .Range("E17") 'Risk-free rate
        S = .Range("E16") 'Current stock price
        K = .Range("E21") 'Strike price
        sigma = .Range("E19") 'Volatility
        v = .Range("E14") 'Remaining vesting period
        t = .Range("E12") '# of years
        step_size = t / .Range("H9")
    End With
'===============================================================================================================
    'CRR model
    up = Exp(sigma * Sqr(step_size))
    down = 1 / up
    p = (Exp((r - d) * step_size) - down) / (up - down)
    dct_f = Exp(-1 * r * step_size) 'Discounting factor
    
    'Modelling stock price process
    ReDim stock_price_array(1 To Int(t / step_size + 1), 1 To Int(t / step_size + 1))
    ReDim payoff_array(1 To Int(t / step_size + 1), 1 To Int(t / step_size + 1))
    
    stock_price_array(1, 1) = S
    j = 2
    Do
        i = 1
        Do
            If i = 1 Then
                stock_price_array(i, j) = stock_price_array(i, j - 1) * up
            Else
                stock_price_array(i, j) = stock_price_array(i - 1, j - 1) * down
            End If
            i = i + 1
        Loop Until i > j
        j = j + 1
    Loop Until j > Int(t / step_size + 1)

    'Calculate Terminal Payoff and Perform Backward Induction
    j = Int(t / step_size + 1)
    Do
        i = 1
        Do
            If j = Int(t / step_size + 1) Then
            'Payoff at terminal
                payoff_array(i, j) = WorksheetFunction.Max(K - stock_price_array(i, j), 0)
            Else
                backward_price = dct_f * (p * payoff_array(i, j + 1) + (1 - p) * payoff_array(i + 1, j + 1))
                exercise_price = K - stock_price_array(i, j)
            'Payoff when the option can be early exercised after time v
                If (j - 1) * step_size > v Then
                    payoff_array(i, j) = WorksheetFunction.Max(backward_price, exercise_price)
                Else
                    payoff_array(i, j) = backward_price
                End If
            End If
            i = i + 1
            Loop Until i > j
        j = j - 1
    Loop Until j < 1
    ws_assp.Range("S2") = payoff_array(1, 1) 'Change according to worksheet
End Sub
