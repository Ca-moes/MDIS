permuta <- function(vec) {
# Lê um vetor e produz todas as permutações

perm <- function (cur, p, n, base, nr) {
if (cur > n){
	print(p)
	nr <- nr+1
	}
else
   for (i in 1:n)
        if (base[i]!=-1) {
		atual <- base[i]
		base[i] <- -1
		p[cur] <- atual
            nr <- perm(cur+1,p,n,base,nr)
		base[i] <- atual
        	}
nr
} #perm

n <- length(vec)
p <- 1:n
nPerm <- perm(1,p,n,vec,0)
print(nPerm)
} #permuta
permuta(c(1,2,3,4))