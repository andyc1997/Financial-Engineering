Option Explicit
Sub American_put_trinomial_tree_model()
'Exercise style: American
'Feature: Put

    Dim d, r, S, K, sigma, up, down, p_up, p_down, p_mid, backward_price, exercise_price, dct_f, decision, t, step_size, tree_end As Double
    Dim i, j As Integer
    Dim stock_price_array As Variant
    Dim payoff_array As Variant
    Dim ws_assp, ws_test As Worksheet
    
    Set ws_assp = Worksheets("Input") 'Change according to worksheet
'===============================================================================================================
'Input: Change according to worksheet
    With ws_assp
        d = .Range("E18") 'Dividend yield
        r = .Range("E17") 'Risk-free rate
        S = .Range("E16") 'Current stock price
        K = .Range("E21") 'Strike price
        sigma = .Range("E19") 'Volatility
        t = .Range("E12") '# of years
        step_size = t / .Range("H9")
    End With
'===============================================================================================================
    'CRR model
    up = Exp(sigma * Sqr(2 * step_size))
    down = 1 / up
    p_up = ((Exp(r * step_size / 2) - Exp(-sigma * Sqr(step_size / 2))) / (Exp(sigma * Sqr(step_size / 2)) - Exp(-sigma * Sqr(step_size / 2)))) ^ 2
    p_down = ((Exp(sigma * Sqr(step_size / 2)) - Exp(r * step_size / 2)) / (Exp(sigma * Sqr(step_size / 2)) - Exp(-sigma * Sqr(step_size / 2)))) ^ 2
    p_mid = 1 - p_up - p_down
    dct_f = Exp(-r * step_size) 'Discounting factor
    tree_end = Int(t / step_size + 1) 'Ending step of the tree
    
    'Modelling stock price process
    ReDim stock_price_array(1 To 2 * tree_end - 1, 1 To tree_end)
    ReDim payoff_array(1 To 2 * tree_end - 1, 1 To tree_end)
    
    stock_price_array(1, 1) = S
    j = 2
    Do
        i = 1
        Do
            If i = 1 Then
                stock_price_array(i, j) = stock_price_array(i, j - 1) * up
            ElseIf i <= 2 * (j - 1) Then
                stock_price_array(i, j) = stock_price_array(i - 1, j - 1)
            Else
                stock_price_array(i, j) = stock_price_array(i - 2, j - 1) * down
            End If
            i = i + 1
        Loop Until i > 2 * (j - 1) + 1
        j = j + 1
    Loop Until j > tree_end

    'Calculate Terminal Payoff and Perform Backward Induction
    j = tree_end
    Do
        i = 1
        Do
            If j = tree_end Then
            'Payoff at terminal
                payoff_array(i, j) = WorksheetFunction.Max(K - stock_price_array(i, j), 0)
            Else
                backward_price = dct_f * (p_up * payoff_array(i, j + 1) + p_mid * payoff_array(i + 1, j + 1) + p_down * payoff_array(i + 2, j + 1))
                exercise_price = K - stock_price_array(i, j)
                payoff_array(i, j) = WorksheetFunction.Max(backward_price, exercise_price)
            End If
            i = i + 1
            Loop Until i > 2 * (j - 1) + 1
        j = j - 1
    Loop Until j < 1
    ws_assp.Range("S2") = payoff_array(1, 1) 'Change according to worksheet
End Sub
