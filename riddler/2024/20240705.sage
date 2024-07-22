x, y, a, b, d = var('x y a b d')

line_of_sight = d + (2-d)*x/(1/2)
reflected_line_of_sight = d + x*(1/2)/(2-d)

assume(d > 0)
assume(d < 1)

answer = solve(reflected_line_of_sight(x=1) == 1, d)[0].rhs()*sqrt(2)

print('answer=',answer,'approximation=',numerical_approx(answer),'latex=',latex(answer))

theta = atan2(b,a)

assume(a, "real")
assume(a > 0)
assume(a < 1)
assume(b, "real")
assume(b > 0)
assume(b < 1)

line_of_sight_b = b + (2-b)*x/(1/2)
phi_b = atan2(2-b,1/2)
reflected_phi_b = 2*theta - phi_b
reflected_line_of_sight_b = b + tan(reflected_phi_b).simplify_full().simplify_full()*x
alpha = solve(reflected_line_of_sight_b(x=1), a)[0].rhs()

line_of_sight_a = (x-a)*2/(1/2-a)
phi_a = atan2(a-1/2,2)
reflected_phi_a = 2*theta - phi_a
beta = solve(reflected_phi_a == pi/2, b)[0].rhs()

# there's some mistake in here, there's no solution for alpha(b=beta) - a == 0
