# Building the SARs
a = sort(sample(1:500, 1000, replace=T))
z = 0.5

c1 = 2
c2 = 5

s1 = c1*(a^z)
s2 = c2*(a^z)

# Graphic representation
par(mfrow=c(1,2))
plot(x=a, type='n', xlim = c(min(a), max(a)), ylim = c(min(c(s1, s2)), max(c(s1, s2))), las=1, xlab = 'a', ylab = 'S')
lines(x=a, y=s1)
lines(x=a, y=s2)

plot(x=a, type='n', xlim = c(min(a), max(a)), ylim = c(min(c(s1, s2)), max(c(s1, s2))), log = 'xy', las=1, xlab = 'log(a)', ylab = 'log(S)')
lines(x=a, y=s1)
lines(x=a, y=s2)

# Log-ratio effect-size
lref = log(s1) - log(s2)
lref[c(1:100, 901:1000)]
