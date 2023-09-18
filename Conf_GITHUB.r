################# GITHUB REPOSITORY ##############################


library(usethis)


#Configurar o GIT
usethis::use_git_config(
  # your name
  user.name = "alv_garcia",
  # your email used in your GitHub account
  user.email = "alvaro.emannuel42@gmail.com"
)


# Criar um token

usethis::create_github_token()

Token<-"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"

# Remover token atual

Sys.unsetenv("GITHUB_PAT")


# Salvar Token

gitcreds::gitcreds_set()


#Testar as configuracoes

usethis::git_sitrep()



# Iniciar o controle de versões no RStudio

usethis::use_git()


# Criar um repositório no GITHUB

usethis::use_github()
