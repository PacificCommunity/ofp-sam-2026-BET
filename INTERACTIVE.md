# MFCL Workflow - Interactive R Scripts

## RStudio에서 라인별로 실행하기

각 스크립트 파일을 RStudio에서 열고 **Ctrl+Enter**로 라인별로 실행하세요.

### 1. Model 실행

**파일: `run_model_condor.R`**

```r
# 설정 부분 수정
model_names <- "base"           # 실행할 모델
# model_names <- c("base", "M1")  # 여러 모델
# model_names <- "all"            # 모든 모델

remote_dir <- NULL              # 자동 생성
# remote_dir <- "develop/test_run"  # 직접 지정

run_local <- FALSE              # 로컬 테스트
# run_local <- TRUE               # Condor 전에 테스트

# 이 아래부터 라인별로 실행 (Ctrl+Enter)
source("config.R")
# ...
```

**사용 예시:**
```r
# RStudio에서 파일 열기
file.edit("run_model_condor.R")

# 1. 설정 수정 (위쪽 부분)
model_names <- "base"
run_local <- FALSE

# 2. 라인별로 실행하면서 확인
source("config.R")
# ... 각 라인 실행

# 3. 또는 전체 실행
source("run_model_condor.R")
```

### 2. Hessian 실행

**파일: `run_hessian_condor.R`**

```r
# 설정
model_names <- "base"
nsplit <- 200                   # part 개수
run_local <- FALSE

# 라인별로 실행
source("config.R")
# ...
```

### 3. Profile 실행

**파일: `run_prof_condor.R`**

```r
# 설정
model_names <- "base"
scalers <- c(100, 90, 80, 70, 60, 50)
run_local <- FALSE

# 라인별로 실행
source("config.R")
# ...
```

### 4. Jitter 실행

**파일: `run_jitter_condor.R`**

```r
# 설정
model_names <- "base"
njitter <- 100
run_local <- FALSE

# 라인별로 실행
source("config.R")
# ...
```

### 5. 결과 가져오기

**파일: `fetch_condor.R`**

```r
# 설정
remote_dir <- "develop/Feb_03_2026_model"
model_names <- "all"
job_types <- "model"
local_dir <- "model"

# 라인별로 실행
source("config.R")
library(CondorBox)
# ...
```

## 실제 workflow 예시

### 시나리오 1: Base 모델 실행

```r
# 1. run_model_condor.R 열기
file.edit("run_model_condor.R")

# 2. 설정 확인/수정
model_names <- "base"
remote_dir <- NULL  # develop/Feb_03_2026_model로 자동 생성
run_local <- FALSE

# 3. 라인별로 실행하면서 확인
source("config.R")  # 설정 로드

# 모델 검증
invalid <- setdiff(model_names, names(MODELS))
# ...

# 요약 보기
cat("Models:", paste(model_names, collapse = ", "), "\n")
# ...

# 제출
cmd <- c("Rscript", "launch.R", "model", ...)
system2(cmd[1], cmd[-1])
```

### 시나리오 2: 로컬 테스트 먼저

```r
# run_model_condor.R에서
run_local <- TRUE  # 로컬 테스트 활성화

# 실행하면 Condor 제출 전에 로컬에서 테스트
source("run_model_condor.R")

# 결과 확인
list.files("model/base")

# 문제 없으면 Condor에 제출
run_local <- FALSE
source("run_model_condor.R")
```

### 시나리오 3: 여러 모델 순서대로

```r
# 1. Base 모델
file.edit("run_model_condor.R")
model_names <- "base"
source("run_model_condor.R")

# 2. 모델 실행 확인 후 M1
model_names <- "M1"
source("run_model_condor.R")

# 3. 또는 한번에
model_names <- c("base", "M1", "M2")
source("run_model_condor.R")
```

### 시나리오 4: 결과 가져오기

```r
# fetch_condor.R 열기
file.edit("fetch_condor.R")

# 설정
remote_dir <- "develop/Feb_03_2026_model"
model_names <- "all"
job_types <- "model"

# 라인별로 실행
source("config.R")
library(CondorBox)

# 명령어 빌드 확인
cmd <- c("Rscript", "fetch_results.R", remote_dir, ...)
cat("Command:", paste(cmd, collapse = " "), "\n")

# 실행
system2(cmd[1], cmd[-1])

# 결과 확인
list.files("model/base")
```

## 장점

### RStudio에서 Interactive하게:
1. **각 라인 확인**: Ctrl+Enter로 한 줄씩 실행하면서 결과 확인
2. **설정 변경**: 위쪽 설정 부분만 수정하고 다시 실행
3. **디버깅**: 문제 발생시 바로 확인하고 수정
4. **단계별 진행**: 천천히 확인하면서 진행 가능

### Command line에서:
```bash
# 전체 실행
Rscript run_model_condor.R
Rscript run_hessian_condor.R
Rscript run_prof_condor.R
Rscript fetch_condor.R
```

## 파일 구조

```
.
├── config.R                  # 중앙 설정 (모델 정의)
├── launch.R                  # 실제 Condor launcher (직접 사용 안함)
├── run_model_condor.R       # Model 실행 스크립트
├── run_hessian_condor.R     # Hessian 실행 스크립트
├── run_prof_condor.R        # Profile 실행 스크립트
├── run_jitter_condor.R      # Jitter 실행 스크립트
├── fetch_condor.R           # 결과 가져오기 스크립트
└── fetch_results.R          # 실제 fetch 구현 (직접 사용 안함)
```

## 모델별 설정 (config.R)

```r
MODELS <- list(
  base = list(
    name = "base",
    description = "Base case model",
    inputs_dir = "mfcl/inputs/2026",
    exec_mode = "par",
    mfcl_version = "2026_01_22_vsn2278",
    par_input = "11.par",
    par_output = "12.par",
    mfcl_switches = "-switch 1 1 1 1"
  ),
  M1 = list(
    name = "M1",
    description = "Sensitivity M1",
    inputs_dir = "mfcl/inputs/2023",
    exec_mode = "doitall",
    mfcl_version = "2023"
  )
)
```

각 모델은 자동으로 올바른 inputs, MFCL 버전, 실행 모드를 사용합니다.
