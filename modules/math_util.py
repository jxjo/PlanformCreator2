#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    Math utility functions

"""

import numpy as np
import math
from bisect import bisect_left



#------------ time to run -----------------------------------


    # from timeit import default_timer as timer
    # start = timer()
    # ...
    # end = timer()
    # print("Time ", end - start)  



#------------ linear interpolation -----------------------------------

def interpolate(x1, x2, y1, y2, x):

    if (x1 -x) == 0.0: return y1
    
    try:
        y = ((y2-y1)/(x2-x1)) * (x-x1) + y1
    except:
        raise ValueError (f"Division by zero in interpolation , x1: {x1}, x2: {x2}")
    return y



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



#------------ derivative -----------------------------------


def derivative1 (x, y):
        """
        evaluate first derivative of polyline (x,y)
        using Backward, center, forward adapted difference approximation
        """

        #     npt = size(x)        
        #     do i = 1, npt
        #       if (i == 1) then                                                 ! forward
        #         derivative1(i) = (y(i+1) - y(i))/ (x(i+1) - x(i)) 
        #       else if (i ==npt) then                                           ! backward
        #         derivative1(i) = (y(i) - y(i-1))/ (x(i) - x(i-1))
        #       else                                                             ! center
        #         h       = x(i+1) - x(i)
        #         h_minus = x(i) - x(i-1)
        #         hr      = h / h_minus 
        #         derivative1(i) = (y(i+1) - hr*hr*y(i-1) -(1-hr*hr)*y(i))/ (h * (1.d0 +hr)) 
        #       end if 
        #     end do

        npt = len(x)
        der1 = np.zeros (npt)
        for i in range(len(x)):
            if i == 0: 
                der1[i] = (y[i+1] - y[i]) / (x[i+1] - x[i])
            elif i == npt-1:
                der1[i] = (y[i] - y[i-1]) / (x[i] - x[i-1])
            else: 
                h = x[i+1] - x[i]
                h_minus = x[i] - x[i-1]
                hr = h / h_minus
                der1[i] = (y[i+1] - hr**2 * y[i-1] -(1 - hr**2) * y[i]) / (h * (1+hr))
        return der1
                     



#------------ find closest   -----------------------------------


def find_closest_index (myList, myNumber) -> int:
    """
    Assumes myList is sorted. Returns index of closest value to myNumber.

    If two numbers are equally close, return index of smallest number.
    """
    pos = bisect_left(myList, myNumber)
    if pos == 0:
        return 0
    if pos == len(myList):
        return -1
    before = myList[pos - 1]
    after = myList[pos]
    if after - myNumber < myNumber - before:
        return pos
    else:
        return pos - 1


#------------ Bisection - find index  -----------------------------------

def bisection(array,value):
    '''Given an ``array`` , and given a ``value`` , returns an index j such that ``value`` is between array[j]
    and array[j+1]. ``array`` must be monotonic increasing. j=-1 or j=len(array) is returned
    to indicate that ``value`` is out of range below and above respectively.'''

    # from https://stackoverflow.com/questions/2566412/find-nearest-value-in-numpy-array
    
    n = len(array)
    if (value < array[0]):
        return -1
    elif (value > array[n-1]):
        return n
    jl = 0                          # Initialize lower
    ju = n-1                        # and upper limits.
    while (ju-jl > 1):              # If we are not yet done,
        jm=(ju+jl) >> 1             # compute a midpoint with a bitshift
        if (value >= array[jm]):
            jl=jm                   # and replace either the lower limit
        else:
            ju=jm                   # or the upper limit, as appropriate.
        # Repeat until the test condition is satisfied.
    if (value == array[0]):         # edge cases at bottom
        return 0
    elif (value == array[n-1]):     # and top
        return n-1
    else:
        return jl


#------------ Bisection - find Root  -----------------------------------


def bisection_fn (f,a,b,N, tolerance=None):
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

def newton(f,Df,x0,epsilon = 10e-8 , max_iter= 50, bounds=None):
    '''Approximate solution of f(x)=0 by Newton's method.

        Implement Newton's method: compute the linear approximation
        of f(x) at xn and find x intercept by the formula
            x = xn - f(xn)/Df(xn)
        Continue until abs(f(xn)) < epsilon and return xn.
        If Df(xn) == 0, raise Error .

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
    bounds : optional - tuple of lower and upper bound of x

    Returns
    -------
    xn : number
    niter : iterations needed 
    '''
    # from https://patrickwalls.github.io/mathematicalpython/root-finding/root-finding/

    xn = x0
    for n in range(0,max_iter):

        if bounds is not None: 
            if   xn > bounds[1]: xn = bounds[1]
            elif xn < bounds[0]: xn = bounds[0]

        fxn = f(xn)

        if abs(fxn) < epsilon:
            break

        Dfxn = Df(xn)
        if Dfxn == 0:
            if xn == 0.0:                       # special case at LE 
                break
            else: 
                raise ValueError ("Newton iteration: Zero derivative. No solution found.")
        xn = xn - fxn/Dfxn

    return xn, n



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

        1D version for speed optimization in simple cases 

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
        if bounds is None: return 0.0
        
        if x < bounds[0] or x > bounds[1]:
            return 9999.9
        else:
            return 0.0

    def fn_penalty (f, x, bounds):
        # return function value - if x outside bounds return penality value 
        if bounds is None: return f(x)
        
        if penalty (x, bounds): 
            fn = penalty (x, bounds)  
        else: 
            fn = f(x)
        return fn


    # init
    prev_best = f(x_start)
    no_improv = 0
    res = [(x_start, prev_best)]

    # mod 
    if penalty(x_start + step, bounds) > 0:
        x = x_start - step 
    else: 
        x = x_start + step

    score = fn_penalty (f, x, bounds) 
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

        # if iters % 20 == 0: 
        #     print("NelderMead  - %3d(%d) score: %.5f  no_improv: %2d thresh: %.5f" %(iters, max_iter, best, no_improv, no_improve_thr))

        if no_improv >= no_improv_break:
            return res[0][0], res[0][1], iters

        # centroid
        x0 = 0.0
        for tup in res[:-1]:
            x0 += tup[0] / (len(res)-1)

        # reflection
        xr = x0 + alpha*(x0 - res[-1][0])
        rscore = fn_penalty (f, xr, bounds)

        if res[0][1] <= rscore < res[-2][1]:
            del res[-1]
            res.append((xr, rscore))
            continue

        # expansion
        if rscore < res[0][1]:
            xe = x0 + gamma*(x0 - res[-1][0])
            escore = fn_penalty (f, xe, bounds)

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
        cscore = fn_penalty (f, xc, bounds)
        if cscore < res[-1][1]:
            del res[-1]
            res.append((xc, cscore))
            continue

        # reduction
        x1 = res[0][0]
        nres = []
        for tup in res:
            redx = x1 + sigma*(tup[0] - x1)
            score = fn_penalty (f, redx, bounds)
            nres.append((redx, score))
        res = nres




def nelder_mead (f, x_start,
                step=0.1, no_improve_thr=10e-10,                # for scalar product
                no_improv_break=12, max_iter=50,
                bounds = None,                                  # extension 
                alpha=1., gamma=2., rho=-0.5, sigma=0.5):
    '''
        Nelder-Mead optimization algorithm to determine the minimum of a fuction

        Allows multi dimensional optimization of scalar function f. 
        For 1D use 'nelder_mead_1D' which should be faster  

        Parameters
        ----------
        f : function to optimize, must return a scalar score
        x_start : np array - initial position
        step    : float - look-around radius in initial step
        no_improv_thr: float - an improvement lower than no_improv_thr
        no_improv_break : int -break after no_improv_break iterations 
        bounds : list of tuple(float) - (min, max) pair for boundary of x. None - no boundary. 
        max_iter : int - always break after this number of iterations.
        alpha, gamma, rho, sigma (floats): parameters of the algorithm
            (see Wikipedia page for reference)
             
        Returns
        -------
        (xbest,score) : np array tuple - x of minimum  , best score 
        niter : int - iterations needed 
        """

    '''
    def penalty (x, bounds):
        if bounds is None: return 0.0
        
        if x < bounds[0] or x > bounds[1]:
            return 9999.9
        else:
            return 0.0

    def fn_penalty (f, x, bounds):
        # return function value - if x outside bounds return penality value 
        if bounds is None: return f(x)
        
        for i in range (len(x)): 
            if penalty (x[i], bounds[i]): 
                return penalty (x[i], bounds[i])
            
        return f(x)


    # init
    dim = len(x_start)
    if bounds is None: 
        bounds = [None] * dim                         # dummy bounds for all x

    prev_best = f(x_start)
    no_improv = 0
    res = [[np.copy(x_start), prev_best]]

    for i in range(dim):
        x = np.copy(x_start)
        if penalty (x[i] + step, bounds[i]):
            x[i] = x[i] - step
        else:
            x[i] = x[i] + step
        score = fn_penalty (f, x, bounds) 
        res.append([x, score])

    # simplex iter
    iters = 0
    # xr, xe, xc = 0, 0, 0 
    while 1:
        # order
        # print ("Iter: ", iters, xr, xe,xc)
        res.sort(key=lambda x: x[1])
        best = res[0][1]

        # break after max_iter
        if max_iter and iters >= max_iter:
            return res[0], iters
        iters += 1

        # break after no_improv_break iterations with no improvement
        # print ('...best so far:', best)

        # if iters % 20 == 0: 
        #     print("  NelderMead-d%d %3d(%d):  Score %.5f  no_improv: %2d(%d)" 
        #           %(dim, iters, max_iter, best, no_improv, no_improv_break))

        if abs(best - prev_best) < no_improve_thr:
            no_improv += 1
        else:
            no_improv = 0
            prev_best = best

        if no_improv >= no_improv_break:
            return res[0], iters

        # centroid
        x0 = [0.0] * dim 
        for tup in res[:-1]:
            for i, c in enumerate(tup[0]):
                x0 [i] += c / (len(res)-1)
        # print ("Centroid ", iters, x0)
        # for i, tup in enumerate(res):
        #     print ("res ", i, tup[0])
        # reflection
        xr = x0 + alpha*(x0 - res[-1][0])
        rscore = fn_penalty (f, xr, bounds) 

        if res[0][1] <= rscore < res[-2][1]:
            del res[-1]
            res.append([xr, rscore])
            continue

        # expansion
        if rscore < res[0][1]:
            xe = x0 + gamma*(x0 - res[-1][0])
            escore = fn_penalty (f, xe, bounds) 
            if escore < rscore:
                del res[-1]
                res.append([xe, escore])
                continue
            else:
                del res[-1]
                res.append([xr, rscore])
                continue

        # contraction
        xc = x0 + rho*(x0 - res[-1][0])
        cscore = fn_penalty (f, xc, bounds) 
        if cscore < res[-1][1]:
            del res[-1]
            res.append([xc, cscore])
            continue

        # reduction
        x1 = res[0][0]
        nres = []
        for tup in res:
            redx = x1 + sigma*(tup[0] - x1)
            score = fn_penalty (f, redx, bounds) 
            nres.append([redx, score])
        res = nres


#--- wrapper functions for nelder_mead minimum------

def nelder_mead_wrap  (fn, xStart,
                no_improve_thr=10e-10,               
                no_improv_break=10, # 5, 
                max_iter=50,
                bounds = None): 
    
    if not bounds is None:
        if xStart < bounds[0] or xStart > bounds[1]:
            raise ValueError ("nelder-mead: Start value %.6f outside bounds" % xStart)
        
        step = (bounds[1] - bounds[0]) / 15.01 #**10.01
    else: 
        step = 0.051

    
    xmin, score, niters =  nelder_mead_1D(fn, xStart, step= step,
                                          no_improve_thr=no_improve_thr, 
                                          no_improv_break=no_improv_break, max_iter=max_iter,  
                                          bounds=bounds)    
    # print ("nelder_mead : ", xmin, score, niters)
    
    # workaround of bug in melder-mead
    # seems to be a float issue as it happens at rounded decimals like 0.61 or 0.8
    # retry with a new xStart-Value solves ... most of the time.

    if niters < max_iter and score > no_improve_thr:
        if not bounds is None: 
            # move start towards bounds0
            xStart_new =  bounds[0] + (xStart - bounds[0]) * 0.9
            # old - xStart_new = (xStart + bounds[0])  / 1.8 # airfoil 1.8 # planform 1.99
            step_new   = (bounds[1] - bounds[0]) / 20.01 # 5.01 
            if xStart_new < bounds[0] or xStart > bounds[1]:
                raise ValueError ("nelder-mead: Start value %.6f outside bounds" % xStart_new)
        else: 
            xStart_new = xStart *  1.012
            step_new   = 0.011
        # try longer 
        no_improv_break = no_improv_break * 3
        # print ("nelder_mead bug:   ", xmin, score, niters, xStart, step, bounds)
        xmin, score, niters =  nelder_mead_1D(fn, xStart_new, step=step_new, 
                                            no_improve_thr=no_improve_thr, 
                                            no_improv_break=no_improv_break, max_iter=max_iter,  
                                            bounds=bounds)    
        # print ("nelder_mead retry: ", xmin, score, niters)
        if score > no_improve_thr:
            raise ValueError ("nelder-mead: Minimum not found for xStart = %.4f" % xStart_new)

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

