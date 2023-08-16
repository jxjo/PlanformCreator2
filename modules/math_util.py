#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    Math utility functions

"""

import numpy as np
import math


#------------ time to run -----------------------------------


    # from timeit import default_timer as timer
    # start = timer()
    # ...
    # end = timer()
    # print("Time ", end - start)  



#------------ panel angles -----------------------------------


def panel_angles (x,y):
    """returns an array of panel angles of polyline x,y - between 160 - 180
    angle[0] and [-1] default to 180° 
    """

    # Xfoil - CANG 

    # C---- go over each point, calculating corner angle
    #       IF(IPRINT.EQ.2) WRITE(*,1050)
    #       DO 30 I=2, N-1
    #         DX1 = X(I) - X(I-1)
    #         DY1 = Y(I) - Y(I-1)
    #         DX2 = X(I) - X(I+1)
    #         DY2 = Y(I) - Y(I+1)
    # C
    # C------ allow for doubled points
    #         IF(DX1.EQ.0.0 .AND. DY1.EQ.0.0) THEN
    #          DX1 = X(I) - X(I-2)
    #          DY1 = Y(I) - Y(I-2)
    #         ENDIF
    #         IF(DX2.EQ.0.0 .AND. DY2.EQ.0.0) THEN
    #          DX2 = X(I) - X(I+2)
    #          DY2 = Y(I) - Y(I+2)
    #         ENDIF
    # C
    #         CROSSP = (DX2*DY1 - DY2*DX1)
    #      &         / SQRT((DX1**2 + DY1**2) * (DX2**2 + DY2**2))
    #         ANGL = ASIN(CROSSP)*(180.0/3.1415926)
    #         IF(IPRINT.EQ.2) WRITE(*,1100) I, X(I), Y(I), ANGL
    #         IF(ABS(ANGL) .GT. ABS(AMAX)) THEN
    #          AMAX = ANGL
    #          IMAX = I
    #         ENDIF
    #    30 CONTINUE

    angles = np.zeros (len(x))
    for i in range(1, len(x)-1):
        dx1 = x[i] - x[i-1] 
        dy1 = y[i] - y[i-1] 
        dx2 = x[i] - x[i+1] 
        dy2 = y[i] - y[i+1] 
        if dx1 != 0.0 and dx2 != 0.0:               # check for pathologic airfoil (blunt le) 
            crossp = (dx2 * dy1 - dy2 * dx1) / math.sqrt ((dx1**2 + dy1**2) * (dx2**2 + dy2**2))
            angles[i] = math.asin(crossp)
        else: 
            angles[i] = 0.0
    angles = 180.0 - angles * (180/np.pi)
    return angles 

#------------ Bisection - find Root  -----------------------------------


def bisection(f,a,b,N, tolerance=None):
    '''Approximate solution of f(x)=0 on interval [a,b] by bisection method.

    Parameters
    ----------
    f : function
        The function for which we are trying to approximate a solution f(x)=0.
    a,b : numbers
        The interval in which to search for a solution. The function returns
        None if f(a)*f(b) >= 0 since a solution is not guaranteed.
    N : (positive) integer
        The number of iterations to implement.

    Returns
    -------
    x_N : number
        The midpoint of the Nth interval computed by the bisection method. The
        initial interval [a_0,b_0] is given by [a,b]. If f(m_n) == 0 for some
        midpoint m_n = (a_n + b_n)/2, then the function returns this solution.
        If all signs of values f(a_n), f(b_n) and f(m_n) are the same at any
        iteration, the bisection method fails and return None.
    '''
    # from https://patrickwalls.github.io/mathematicalpython/root-finding/root-finding/

    if   abs(f(a)) < 10e-10 : return a, 0
    elif abs(f(b)) < 10e-10 : return b, 0
    elif f(a)*f(b) > 0:
        print("Bisection method fails.", a, f(a), b, f(b))
        return None, 0
    a_n = a
    b_n = b
    for n in range(1,N+1):
        m_n = (a_n + b_n)/2
        if not tolerance is None and abs(a_n - b_n) < tolerance:
            return m_n, n 
        f_m_n = f(m_n)
        if f(a_n)*f_m_n < 0:
            a_n = a_n
            b_n = m_n
        elif f(b_n)*f_m_n < 0:
            a_n = m_n
            b_n = b_n
        elif f_m_n == 0:
            print("Found exact solution.")
            return m_n, n
        else:
            print("Bisection method fails.")
            return None

    return (a_n + b_n)/2, n


#------------ Newton iteration - find Root  -----------------------------------

def newton(f,Df,x0,epsilon,max_iter):
    '''Approximate solution of f(x)=0 by Newton's method.

    Parameters
    ----------
    f : function
        Function for which we are searching for a solution f(x)=0.
    Df : function
        Derivative of f(x).
    x0 : number
        Initial guess for a solution f(x)=0.
    epsilon : number
        Stopping criteria is abs(f(x)) < epsilon.
    max_iter : integer
        Maximum number of iterations of Newton's method.

    Returns
    -------
    xn : number
        Implement Newton's method: compute the linear approximation
        of f(x) at xn and find x intercept by the formula
            x = xn - f(xn)/Df(xn)
        Continue until abs(f(xn)) < epsilon and return xn.
        If Df(xn) == 0, return None. If the number of iterations
        exceeds max_iter, then return None.
    '''
    # from https://patrickwalls.github.io/mathematicalpython/root-finding/root-finding/

    xn = x0
    for n in range(0,max_iter):
        fxn = f(xn)
        if abs(fxn) < epsilon:
            print('Found solution after',n,'iterations.')
            return xn
        Dfxn = Df(xn)
        if Dfxn == 0:
            print('Zero derivative. No solution found.')
            return None
        xn = xn - fxn/Dfxn
    print('Exceeded maximum iterations. No solution found.')
    return None

# ---------------------------------------------------------------------------
# (c) https://github.com/fchollet/nelder-mead 
# 
# Pure Python/Numpy implementation of the Nelder-Mead optimization algorithm
# to determine the minimum of a fuction - replaces fmin and brentq from scipy

def nelder_mead_1D (f, x_start,
                step=0.1, no_improve_thr=10e-10,                # for scalar product - org: no_improve_thr=10e-8,
                no_improv_break=12, max_iter=50,
                bounds = None,                                  # extension 
                alpha=1., gamma=2., rho=-0.5, sigma=0.5):
    '''
        1D Nelder-Mead optimization algorithm to determine the minimum of a fuction

        Parameters
        ----------
        f : function to optimize, must return a scalar score
        x_start : float - initial position
        step    : float - look-around radius in initial step
        no_improv_thr: float - an improvement lower than no_improv_thr
        no_improv_break : int -break after no_improv_break iterations 
        bounds : tuple(float) - (min, max) pair for boundary of x. None - no boundary. 
             
        Returns
        -------
        xbest : x of minimum  
        score : best score  
        niter : int - iterations needed 
        """

    '''
    def penalty (x, bounds):
        "jx-extension: returns a penalty of 1 if x is outside bounds"
        if bounds is None: 
            return 0.0 
        elif x < bounds[0] or x > bounds[1]: 
            return 999.0 
        else: 
            return 0.0 

    # init
    prev_best = f(x_start)
    no_improv = 0
    res = [(x_start, prev_best)]

    # mod 
    if penalty(x_start + step, bounds) > 0:
        x = x_start - step 
    else: 
        x = x_start + step

    score = f(x) + penalty(x, bounds)
    res.append((x, score))

    # simplex iter
    iters = 0
    while 1:
        # order
        res.sort(key=lambda score: score[1])
        best = res[0][1]

        # break after max_iter
        if max_iter and iters >= max_iter:
            return res[0][0], res[0][1], iters
        iters += 1

        # break after no_improv_break iterations with no improvement
        # print ('...best so far:', best)

        if abs(best - prev_best) < no_improve_thr:
            no_improv += 1
        else:
            no_improv = 0
            prev_best = best

        if no_improv >= no_improv_break:
            return res[0][0], res[0][1], iters

        # centroid
        x0 = 0.0
        for tup in res[:-1]:
            x0 += tup[0] / (len(res)-1)

        # reflection
        xr = x0 + alpha*(x0 - res[-1][0])
        if penalty(xr, bounds):
            rscore = penalty(xr, bounds)                    # xr out of bounds 
        else: 
            rscore = f(xr) + penalty(xr, bounds)

        if res[0][1] <= rscore < res[-2][1]:
            del res[-1]
            res.append((xr, rscore))
            continue

        # expansion
        if rscore < res[0][1]:
            xe = x0 + gamma*(x0 - res[-1][0])
            escore = f(xe) + penalty(xe, bounds)
            if escore < rscore:
                del res[-1]
                res.append((xe, escore))
                continue
            else:
                del res[-1]
                res.append((xr, rscore))
                continue

        # contraction
        xc = x0 + rho*(x0 - res[-1][0])
        cscore = f(xc) + penalty(xc, bounds)
        if cscore < res[-1][1]:
            del res[-1]
            res.append((xc, cscore))
            continue

        # reduction
        x1 = res[0][0]
        nres = []
        for tup in res:
            redx = x1 + sigma*(tup[0] - x1)
            score = f(redx) + penalty(redx, bounds)
            nres.append((redx, score))
        res = nres


#--- wrapper functions for nelder_mead minimum------

def nelder_mead_wrap  (fn, xStart,
                no_improve_thr=10e-10,               
                no_improv_break=12, max_iter=50,
                bounds = None): 
    
    if not bounds is None:
        if xStart < bounds[0] or xStart > bounds[1]:
            raise ValueError ("nelder-mead: Start value %.46f outside bounds" % xStart)

    
    xmin, score, niters =  nelder_mead_1D(fn, xStart, 
                                          no_improve_thr=no_improve_thr, 
                                          no_improv_break=no_improv_break, max_iter=max_iter,  
                                          bounds=bounds)    
    # workaround of bug in melder-mead
    # seems to be a float issue as it happens at rounded decimals like 0.61 or 0.8
    # retry with a new xStart-Value solves ... most of the time.

    if niters < max_iter and score > no_improve_thr:
        if not bounds is None: 
            xStart = (xStart + bounds[0]) / 2.01
        else: 
            xStart = xStart *  1.012
        # print ("nelder_mead bug:   ", xmin, score, niters)
        xmin, score, niters =  nelder_mead_1D(fn, xStart, 
                                            no_improve_thr=no_improve_thr, 
                                            no_improv_break=no_improv_break, max_iter=max_iter,  
                                            bounds=bounds)    
        # print ("nelder_mead retry: ", xmin, score, niters)
        if score > no_improve_thr:
            raise ValueError ("nelder-mead: Minimum not found for xStart = %.4f" % xStart)

    return xmin


def findMin (fn, xStart, bounds=None, no_improve_thr=10e-10): 
    xmin =  nelder_mead_wrap (fn, xStart, bounds=bounds, no_improve_thr=no_improve_thr)    
    return xmin 

def findMax (fn, xStart, bounds=None): 
    xmax =  nelder_mead_wrap(lambda x: - (fn(x)), xStart, bounds=bounds)    
    return xmax 

def findRoot (fn, xStart, bounds=None, no_improve_thr=10e-12): 
    xRoot =  nelder_mead_wrap(lambda x: abs(fn(x)), xStart, no_improve_thr=no_improve_thr,  bounds=bounds)    
    return xRoot 



#------------ cosinus distribution -----------------------------------


def cosinus_distribution (nPoints, le_bunch, te_bunch):
    """ 
    returns an array with cosinues distributed values 0..1
    
    Args: 
    nPoints : new number of coordinate points
    le_bunch : 0..1  where 1 is the full cosinus bunch at leading edge - 0 no bunch 
    te_bunch : 0..1  where 1 is the full cosinus bunch at trailing edge - 0 no bunch 
    """

    xfacStart = 0.1 - le_bunch * 0.1
    xfacEnd   = 0.7 + te_bunch * 0.3 

    xfacStart = max(0.0, xfacStart)
    xfacStart = min(0.5, xfacStart)
    xfacEnd   = max(0.5, xfacEnd)
    xfacEnd   = min(1.0, xfacEnd)

    if xfacStart >= xfacEnd: raise ValueError ("Airfoil cosinus-distribution: start > end")

    beta = np.linspace(xfacStart, xfacEnd , nPoints) * np.pi
    xnew = (1.0 - np.cos(beta)) * 0.5

    # normalize to 0..1
    xmin = np.amin(xnew)
    xmax = np.amax(xnew) 
    xnew = (xnew - xmin) / (xmax-xmin)

    # ensure 0.0 and 1.0 
    xnew[0]  = xnew[0].round(10)
    xnew[-1] = xnew[-1].round(10)

    return xnew

