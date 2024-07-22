a1, a2, b = var('a1 a2 b')

lose_spin1 = 1 - a1 + integrate(1-a1, b, 0, a1)

lose_spin2 = a1 + integrate(1-(a1+a2)^2, a2, 0, 1-a1)

assume(a1, "real")

x = solve(lose_spin1 == lose_spin2, a1)[0].rhs().simplify_full()

lose = integrate(lose_spin1, a1, x, 1) + integrate(lose_spin2, a1, 0, x)

answer = 1-lose

print('answer=',answer,'approximation=',numerical_approx(answer),'latex=',latex(answer))

y = var('y')

ec_lose = \
    integrate(a1 + integrate(1-x^2*(a1+a2)^2/2 - (a1+a2)^4/2, a2, 0, x-a1) \
                 + integrate(1-(a1+a2)^4, a2, x-a1, 1-a1), a1, 0, x) \
  + integrate(a1 + integrate(1-(a1+a2)^4, a2, 0, 1-a1), a1, x, y) \
  + integrate(1-a1^4, a1, y, 1)

extra_credit_answer = numerical_approx(1-ec_lose(y=find_root(derivative(ec_lose, y), 0, 1)))

print('extra_credit_answer approximation=',extra_credit_answer)

figure_1 = plot(ec_lose, y, 0, 1)

print('figure_1')
