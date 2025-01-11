def minimum_contestants(k, n):
    if k == 1:
        return n
    return minimum_contestants(k-1, ceil(n/k)*k)

from functools import cache

@cache
def average_rounds(n):
    if n == 0:
        return 0
    lhs = 1
    rhs = 1
    for k in [1..10]:
        m = k*floor(n/k)
        if m == n:
            lhs -= 1/10
        else:
            rhs += average_rounds(m)/10
    return rhs/lhs

def handsome(n, maximum_continuations, continuations, available_digits):
    if n == 0:
        if len(available_digits) == 0:
            yield continuations
        return
    if maximum_continuations < 1:
        return
    for digit_1 in available_digits:
        sum_1 = digit_1
        digits_1 = frozenset({digit_1})
        if mod(n, 10) == mod(sum_1, 10):
            for h in handsome((n - sum_1)//10, 1,
                              [digits_1] + continuations,
                              available_digits - digits_1):
                yield h
        if maximum_continuations < 2:
            continue
        for digit_2 in available_digits:
            if digit_2 <= digit_1:
                continue
            sum_2 = sum_1 + digit_2
            digits_2 = digits_1 | frozenset({digit_2})
            if mod(n, 10) == mod(sum_2, 10):
                for h in handsome((n - sum_2)//10, 2,
                                  [digits_2] + continuations,
                                  available_digits - digits_2):
                    yield h
            if maximum_continuations < 3:
                continue
            for digit_3 in available_digits:
                if digit_3 <= digit_2:
                    continue
                sum_3 = sum_2 + digit_3
                digits_3 = digits_2 | frozenset({digit_3})
                if mod(n, 10) == mod(sum_3, 10):
                    for h in handsome((n - sum_3)//10, 3,
                                      [digits_3] + continuations,
                                      available_digits - digits_3):
                        yield h
                if maximum_continuations < 4:
                    continue
                for digit_4 in available_digits:
                    if digit_4 <= digit_3:
                        continue
                    sum_4 = sum_3 + digit_4
                    digits_4 = digits_3 | frozenset({digit_4})
                    if mod(n, 10) == mod(sum_4, 10):
                        for h in handsome((n - sum_4)//10, 4,
                                          [digits_4] + continuations,
                                          available_digits - digits_4):
                            yield h
                    if maximum_continuations < 5:
                        continue
                    for digit_5 in available_digits:
                        if digit_5 <= digit_4:
                            continue
                        sum_5 = sum_4 + digit_5
                        digits_5 = digits_4 | frozenset({digit_5})
                        if mod(n, 10) == mod(sum_5, 10):
                            for h in handsome((n - sum_5)//10, 5,
                                              [digits_5] + continuations,
                                              available_digits - digits_5):
                                yield h
                        if maximum_continuations < 6:
                            continue
                        for digit_6 in available_digits:
                            if digit_6 <= digit_5:
                                continue
                            sum_6 = sum_5 + digit_6
                            digits_6 = digits_5 | frozenset({digit_6})
                            if mod(n, 10) == mod(sum_6, 10):
                                for h in handsome((n - sum_6)//10, 6,
                                                  [digits_6] + continuations,
                                                  available_digits - digits_6):
                                    yield h
                            if maximum_continuations < 7:
                                continue
                            for digit_7 in available_digits:
                                if digit_7 <= digit_6:
                                    continue
                                sum_7 = sum_6 + digit_7
                                digits_7 = digits_6 | frozenset({digit_7})
                                if mod(n, 10) == mod(sum_7, 10):
                                    for h in handsome((n - sum_7)//10, 7,
                                                      [digits_7] + continuations,
                                                      available_digits - digits_7):
                                        yield h
                                # no need to go to 8 or beyond when
                                # dealing with numbers of up to 4 digits

def handsome_combinations(digits):
    combos = 1
    for i in [0..len(digits)-2]:
        n = len(digits[i])
        m = len(digits[i+1])
        if m > n and i+2 < len(digits) and 0 in digits[i+1]:
            combos *= n*factorial(m-1)//factorial(m-n)
        else:
            combos *= factorial(m)//factorial(m-n)
    return combos

def how_handsome(n):
    combos = 0
    for h in handsome(n, 7, [], frozenset([0..9])):
        combos += handsome_combinations(h)
    return combos
