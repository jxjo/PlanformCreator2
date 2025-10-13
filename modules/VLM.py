#!/usr/bin/env python2
# -*- coding: utf-8 -*-
import copy
import numpy as np


def calc_induced_velocities(aerogrid, Ma):
    #
    #                   l_2
    #             4 o---------o 3
    #               |         |
    #  u -->    b_1 | l  k  j | b_2
    #               |         |
    #             1 o---------o 2
    #         y         l_1
    #         |
    #        z.--- x

    # define downwash location (3/4 chord and half span of the aero panel)
    P0 = aerogrid['offset_j']
    # define vortex location points
    P1 = aerogrid['offset_P1']
    P3 = aerogrid['offset_P3']
    # P2 = mid-point between P1 and P3, not used
    # normal vector part in vertical direction
    n_hat_w = np.array(aerogrid['N'][:, 2], ndmin=2).T.repeat(aerogrid['n'], axis=1)
    # normal vector part in lateral direction
    n_hat_wl = np.array(aerogrid['N'][:, 1], ndmin=2).T.repeat(aerogrid['n'], axis=1)

    # divide x coordinates with beta
    # See Hedman 1965.
    # However, Hedman divides by beta^2 ... why??
    beta = (1 - (Ma ** 2.0)) ** 0.5
    P0[:, 0] = P0[:, 0] / beta
    P1[:, 0] = P1[:, 0] / beta
    P3[:, 0] = P3[:, 0] / beta

    # See Katz & Plotkin, Chapter 10.4.5
    # get r1,r2,r0
    r1x = np.array(P0[:, 0], ndmin=2).T - np.array(P1[:, 0], ndmin=2)
    r1y = np.array(P0[:, 1], ndmin=2).T - np.array(P1[:, 1], ndmin=2)
    r1z = np.array(P0[:, 2], ndmin=2).T - np.array(P1[:, 2], ndmin=2)

    r2x = np.array(P0[:, 0], ndmin=2).T - np.array(P3[:, 0], ndmin=2)
    r2y = np.array(P0[:, 1], ndmin=2).T - np.array(P3[:, 1], ndmin=2)
    r2z = np.array(P0[:, 2], ndmin=2).T - np.array(P3[:, 2], ndmin=2)

    # Step 1
    r1Xr2_x = r1y * r2z - r1z * r2y
    r1Xr2_y = -r1x * r2z + r1z * r2x  # Plus-Zeichen Abweichung zu Katz & Plotkin ??
    r1Xr2_z = r1x * r2y - r1y * r2x
    mod_r1Xr2 = (r1Xr2_x ** 2.0 + r1Xr2_y ** 2.0 + r1Xr2_z ** 2.0) ** 0.5
    # Step 2
    r1 = (r1x ** 2.0 + r1y ** 2.0 + r1z ** 2.0) ** 0.5
    r2 = (r2x ** 2.0 + r2y ** 2.0 + r2z ** 2.0) ** 0.5
    # Step 4
    r0r1 = (P3[:, 0] - P1[:, 0]) * r1x + (P3[:, 1] - P1[:, 1]) * r1y + (P3[:, 2] - P1[:, 2]) * r1z
    r0r2 = (P3[:, 0] - P1[:, 0]) * r2x + (P3[:, 1] - P1[:, 1]) * r2y + (P3[:, 2] - P1[:, 2]) * r2z
    # Step 5
    gamma = np.ones((aerogrid['n'], aerogrid['n']))
    D1_base = gamma / 4.0 / np.pi / mod_r1Xr2 ** 2.0 * (r0r1 / r1 - r0r2 / r2)
    D1_v = r1Xr2_y * D1_base
    D1_w = r1Xr2_z * D1_base
    # Step 3
    epsilon = 10e-6
    ind = np.where(r1 < epsilon)[0]
    D1_v[ind] = 0.0
    D1_w[ind] = 0.0

    ind = np.where(r2 < epsilon)[0]
    D1_v[ind] = 0.0
    D1_w[ind] = 0.0

    ind = np.where(mod_r1Xr2 < epsilon)
    D1_v[ind] = 0.0
    D1_w[ind] = 0.0

    # get final D1 matrix
    # D1 matrix contains the perpendicular component of induced velocities at all panels.
    # For wing panels, it's the z component of induced velocities (D1_w) while for
    # winglets, it's the y component of induced velocities (D1_v)
    D1 = D1_w * n_hat_w + D1_v * n_hat_wl

    # See Katz & Plotkin, Chapter 10.4.7
    # induced velocity due to inner semi-infinite vortex line
    d2 = (r1y ** 2.0 + r1z ** 2.0) ** 0.5
    cosBB1 = 1.0
    cosBB2 = -r1x / r1
    cosGamma = r1y / d2
    sinGamma = -r1z / d2

    D2_base = -(1.0 / (4.0 * np.pi)) * (cosBB1 - cosBB2) / d2
    D2_v = sinGamma * D2_base
    D2_w = cosGamma * D2_base

    ind = np.where((r1 < epsilon) + (d2 < epsilon))[0]
    D2_v[ind] = 0.0
    D2_w[ind] = 0.0

    D2 = D2_w * n_hat_w + D2_v * n_hat_wl

    # induced velocity due to outer semi-infinite vortex line
    d3 = (r2y ** 2.0 + r2z ** 2.0) ** 0.5
    cosBB1 = r2x / r2
    cosBB2 = -1.0
    cosGamma = -r2y / d3
    sinGamma = r2z / d3

    D3_base = -(1.0 / (4.0 * np.pi)) * (cosBB1 - cosBB2) / d3
    D3_v = sinGamma * D3_base
    D3_w = cosGamma * D3_base

    ind = np.where((r2 < epsilon) + (d3 < epsilon))[0]
    D3_v[ind] = 0.0
    D3_w[ind] = 0.0

    D3 = D3_w * n_hat_w + D3_v * n_hat_wl

    return D1, D2, D3


def mirror_aerogrid_xz(aerogrid):
    tmp = copy.deepcopy(aerogrid)
    # mirror y-coord
    tmp['offset_j'][:, 1] = -tmp['offset_j'][:, 1]
    tmp['offset_k'][:, 1] = -tmp['offset_k'][:, 1]
    tmp['offset_l'][:, 1] = -tmp['offset_l'][:, 1]
    tmp['offset_P1'][:, 1] = -tmp['offset_P1'][:, 1]
    tmp['offset_P3'][:, 1] = -tmp['offset_P3'][:, 1]
    tmp['N'][:, 1] = -aerogrid['N'][:, 1]
    tmp['N'][:, 2] = -aerogrid['N'][:, 2]
    # assemble both grids
    aerogrid_xzsym = {'offset_j': np.vstack((aerogrid['offset_j'], tmp['offset_j'])),
                      'offset_k': np.vstack((aerogrid['offset_k'], tmp['offset_l'])),
                      'offset_l': np.vstack((aerogrid['offset_k'], tmp['offset_l'])),
                      'offset_P1': np.vstack((aerogrid['offset_P1'], tmp['offset_P1'])),
                      'offset_P3': np.vstack((aerogrid['offset_P3'], tmp['offset_P3'])),
                      'N': np.vstack((aerogrid['N'], tmp['N'])),
                      'A': np.hstack((aerogrid['A'], tmp['A'])),
                      'l': np.hstack((aerogrid['l'], tmp['l'])),
                      'n': aerogrid['n'] * 2,
                      }
    return aerogrid_xzsym


def calc_Ajj(aerogrid, Ma):
    D1, D2, D3 = calc_induced_velocities(aerogrid, Ma)
    # define area, chord length and spann of each panel
    A = aerogrid['A']
    chord = aerogrid['l']
    span = A / chord
    # total D
    D = D1 + D2 + D3
    Ajj = D * 0.5 * A / span
    Bjj = (D2 + D3) * 0.5 * A / span
    return Ajj, Bjj


def calc_Qjj(aerogrid, Ma, xz_symmetry=False):
    '''
    Symmetry about xz-plane:
    Only the right hand side is give. The (missing) left hand side is created virtually using mirror_aerogrid_xz().
    The AIC matrix is calculated for the whole system and can be partitioned into terms:
    AIC = |RR|LR|
          |RL|LL|
    with
    RR - influence of right side onto right side,
    LL - influence of left side onto left side,
    RL and LR - influence from left side onto right side. Due to symmetry, both term are identical.
    Then, for symmetric motions: AIC_sym  = RR - LR
    And, for asymmetric motions: AIC_asym = RR + LR  ??? -> to be checked !!!
    '''
    if xz_symmetry:
        n = aerogrid['n']
        aerogrid = mirror_aerogrid_xz(aerogrid)

    # The function calc_Ajj() is Mach number dependent, which involves a scaling of the aerogrid in x-direction.
    # To make sure that the geometrical scaling has no effect on the following calculations, a 'fresh' a copy of the aerogrid,
    # created with copy.deepcopy(), is handed over.
    Ajj, Bjj = calc_Ajj(aerogrid=copy.deepcopy(aerogrid), Ma=Ma)
    Qjj = -np.linalg.inv(Ajj)
    if xz_symmetry:
        return Qjj[0:n, 0:n] - Qjj[n:2 * n, 0:n], Bjj[0:n, 0:n] - Bjj[n:2 * n, 0:n]
    return Qjj, Bjj


def calc_Qjjs(aerogrid, Ma, xz_symmetry=False):
    Qjj = np.zeros((len(Ma), aerogrid['n'], aerogrid['n']))  # dim: Ma,n,n
    Bjj = np.zeros((len(Ma), aerogrid['n'], aerogrid['n']))  # dim: Ma,n,n
    for i, i_Ma in enumerate(Ma):
        Qjj[i, :, :], Bjj[i, :, :] = calc_Qjj(aerogrid, i_Ma, xz_symmetry)
    return Qjj, Bjj


def calc_Gamma(aerogrid, Ma, xz_symmetry=False):
    if xz_symmetry:
        n = aerogrid['n']
        aerogrid = mirror_aerogrid_xz(aerogrid)
    D1, D2, D3 = calc_induced_velocities(aerogrid, Ma)
    # total D
    Gamma = -np.linalg.inv((D1 + D2 + D3))
    Q_ind = D2 + D3

    if xz_symmetry:
        return Gamma[0:n, 0:n] - Gamma[n:2 * n, 0:n], Q_ind[0:n, 0:n] - Q_ind[n:2 * n, 0:n]
    return Gamma, Q_ind


def calc_Gammas(aerogrid, Ma, xz_symmetry=False):
    Gamma = np.zeros((len(Ma), aerogrid['n'], aerogrid['n']))  # dim: Ma,n,n
    Q_ind = np.zeros((len(Ma), aerogrid['n'], aerogrid['n']))  # dim: Ma,n,n
    for i, i_Ma in enumerate(Ma):
        Gamma[i, :, :], Q_ind[i, :, :] = calc_Gamma(aerogrid, i_Ma, xz_symmetry)
    return Gamma, Q_ind
