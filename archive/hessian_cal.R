



# 파일 연결 생성
con <- file("bet.hes", "rb")

# 데이터 읽기
data <- readBin(con, what = numeric(), n = 1e6)

# 파일 닫기
close(con)




# Convert *.hes to text format
cat("* Fetching Hessian file ... ")
null <- file.copy(file.path(penguin, "bet.hes"), ".")
cat("done\n* Converting Hessian file to text format ...")
system2(path.expand("~/admb/bin/ad2csv"), 
        args = "bet.hes", 
        stdout = "hessian.csv")

cat("done\n* Reading Hessian values ... ")
H <- as.matrix(read.csv("hessian.csv", header=FALSE))
cat("done\n* Computing inverse Hessian ... ")
invH <- solve(H)
cat("done\n")
hes <- diag(invH)













# Read files
cat("* Reading MFCL files ... ")
par <- read.MFCLPar("11.par")
hd <- scan("bet_hess_diag", quiet=TRUE)
hid <- read.table("bet_hess_inv_diag")[,2]
ns <- scan("bet_new.std", quiet=TRUE)
phd <- scan("bet_pos_hess_diag", quiet=TRUE)[-1]
phid2 <- read.table("bet_pos_hess_inv_diag2")[,2]
pns <- scan("bet_pos_new.std", quiet=TRUE)
pshd2 <- read.table("bet_pos_sqrt_hess_diag2")[,2]
phcov <- as.matrix(read.table("bet_pos_hess_cov"))
cov <- diag(phcov)
cat("done\n")

# Convert *.hes to text format
cat("done\n* Converting Hessian file to text format ...")
system2(path.expand("~/admb/bin/ad2csv"), 
        args = "bet.hes", 
        stdout = "hessian.csv")

cat("done\n* Reading Hessian values ... ")
H <- as.matrix(read.csv("hessian.csv", header=FALSE))

# 고유값 계산
eigenvalues <- eigen(H)$values

# 양정부호 확인
is_positive_definite <- all(eigenvalues > 0)

if(is_positive_definite) {
  cat("✓ Hessian is POSITIVE DEFINITE\n")
  cat("  All eigenvalues are positive\n")
} else {
  cat("✗ Hessian is NOT positive definite\n")
}

cat("done\n* Computing inverse Hessian ... ")
invH <- solve(H)
cat("done\n")
hes <- diag(invH)

# Parameter estimates
logM <- drop(log_m(par))[1,1]
vonB <- unname(growth(par)[2:3,1])  # L1 not estimated
vpar <- unname(growth_var_pars(par)[,1])
est <- c(logM, vonB, vpar)

# Table
tab <- data.frame(final.par=est)
row.names(tab) <- c("logM", "L2", "K", "SD1", "SD2")
npar <- nrow(tab)
tab$bet_hess_diag <- tail(hd, npar)
tab$bet_hess_inv_diag <- tail(hid, npar)
tab$bet_new.std <- tail(ns, npar)
tab$bet_pos_hess_diag <- tail(phd, npar)
tab$bet_pos_hess_inv_diag2 <- tail(phid2, npar)
tab$bet_pos_new.std <- tail(pns, npar)
tab$bet_pos_sqrt_hess_diag2 <- tail(pshd2, npar)
tab$cov <- tail(cov, npar)
tab$hes <- tail(hes, npar)
tab <- as.data.frame(t(tab))

# Formatted output
write.csv(tab, "hessian_files.csv")






# 음수 고유값 확인
eigenvalues <- eigen(H, symmetric = TRUE)$values

# 음수 고유값과 그 크기
neg_eig <- eigenvalues[eigenvalues < 0]
cat("Negative eigenvalue:", neg_eig, "\n")

# 절대값으로 비교
cat("Min eigenvalue:", min(eigenvalues), "\n")
cat("Max eigenvalue:", max(eigenvalues), "\n")
cat("Ratio:", abs(min(eigenvalues)) / max(eigenvalues), "\n")

# 음수 고유값이 작은지 확인
if(abs(min(eigenvalues)) < 1e-6 * max(eigenvalues)) {
  cat("⚠ Negative eigenvalue is numerically small\n")
  cat("  -> Likely numerical precision issue\n")
}

