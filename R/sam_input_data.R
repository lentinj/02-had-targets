ft_sam_cn <- function(
  model_dat,
  minage,
  maxage
) {
  cn <-
    model_dat |>
    dplyr::mutate(
      catch = dplyr::case_when(
        is.na(catch) | catch == 0 ~ NA_real_,
        TRUE ~ catch
      )
    ) |>
    SAMutils::sam.input(
      "catch",
      age_range = as.numeric(minage:maxage),
      na.fill = NA_real_
    )

  ## blot out the incomplete catches in the interim year
  cn[dim(cn)[1], ] <- NA_real_
  return(cn)
}


ft_sam_cw <- function(
  model_dat,
  minage,
  maxage,
  tyr
) {
  cw <-
    model_dat |>
    dplyr::mutate(
      catch = dplyr::case_when(is.na(catch) ~ NA_real_, TRUE ~ catch)
    ) |>
    SAMutils::sam.input(
      "catch_weight",
      age_range = as.numeric(minage:maxage),
      tail_f = function(x, ...) dplyr::first(x),
      na.fill = 0
    )
  cw[dim(cw)[1], ] <- cw[dim(cw)[1] - 1, ]
  cw <- cw / 1000
  return(cw)
}


ft_sam_smb <- function(
  model_dat,
  minage,
  maxage
) {
  smb <-
    model_dat |>
    dplyr::filter(year > 1984) |>
    SAMutils::sam.input(
      "smb",
      age_range = as.numeric(minage:maxage),
      time_window = c(0.15, 0.2),
      na.fill = NA_real_
    ) |>
    (\(x) (ifelse(x < 0, NA_real_, x) * 1e3))()
  attributes(smb)$time <- c(0.15, 0.2)
  return(smb)
}


ft_sam_smh <- function(
  model_dat,
  minage,
  maxage
) {
  smh <-
    model_dat |>
    dplyr::filter(year > 1994) |>
    dplyr::mutate(smh = ifelse(age > 10, NA_real_, smh)) |>
    SAMutils::sam.input(
      "smh",
      age_range = as.numeric(minage:maxage),
      #time_window = c(0.15, 0.2),
      na.fill = NA_real_
    ) |>
    (\(x) (ifelse(x < 0, NA_real_, x) * 1e3))()
  attributes(smh)$time <- c(0.75, 0.8)
  smh['2011', ] <- NA_real_
  return(smh)
}

ft_sam_sw <- function(
  model_dat,
  minage,
  maxage
) {
  sw <-
    model_dat |>
    SAMutils::sam.input(
      "stock_weight",
      age_range = as.numeric(minage:maxage),
      tail_f = function(x, ...) dplyr::first(x),
      na.fill = 0.001
    )
  sw <- sw / 1000
  return(sw)
}


ft_sam_mo <- function(
  model_dat,
  minage,
  maxage
) {
  model_dat |>
    SAMutils::sam.input(
      "maturity",
      age_range = as.numeric(minage:maxage),
      tail_f = max,
      na.fill = 1
    )
}


ft_sam_lf <- function(
  model_dat,
  cn,
  minage,
  maxage
) {
  lf <- array(1, dim = dim(cn))
  dimnames(lf) <- dimnames(cn)
  return(lf)
}


ft_sam_pf <- function(
  model_dat,
  cn,
  minage,
  maxage
) {
  pf <- array(0.4, dim = dim(cn))
  dimnames(pf) <- dimnames(cn)
  return(pf)
}


ft_sam_pm <- function(
  model_dat,
  cn,
  minage,
  maxage
) {
  pm <- array(0.3, dim = dim(cn))
  dimnames(pm) <- dimnames(cn)
  return(pm)
}


ft_sam_nm <- function(
  model_dat,
  minage,
  maxage
) {
  model_dat |>
    SAMutils::sam.input(
      "M",
      age_range = as.numeric(1:maxage),
      tail_f = mean,
      na.fill = 0.2
    )
}


ft_sam_dat <- function(
  cn,
  cw,
  smb,
  smh,
  sw,
  mo,
  lf,
  pf,
  pm,
  nm,
  tyr
) {
  dat <- stockassessment::setup.sam.data(
    surveys = list(spring = smb, autumn = smh),
    residual.fleet = cn,
    prop.mature = mo,
    stock.mean.weight = sw,
    catch.mean.weight = cw,
    dis.mean.weight = cw,
    land.mean.weight = cw,
    prop.f = pf,
    prop.m = pm,
    natural.mortality = nm,
    land.frac = lf
  )

  return(dat)
}

ft_sam_conf <- function(dat) {
  within(stockassessment::defcon(dat), {
    maxAgePlusGroup = c(1, 1, 1)
    stockRecruitmentModelCode = 3
    keyLogFsta[1, ] = 0:(length(keyLogFsta[1, ]) - 1)

    predVarObsLink[1, !is.na(predVarObsLink[1, ])] <- 0 #-1 #0 #0:(sum(predVarObsLink[1,]==-1,na.rm=TRUE)-1)
    predVarObsLink[2, !is.na(predVarObsLink[2, ])] <- max(
      predVarObsLink[1, ],
      na.rm = TRUE
    ) +
      1 #+ c(rep(0,7),rep(1,5))
    predVarObsLink[3, !is.na(predVarObsLink[3, 1:6])] <- max(
      predVarObsLink[2, ],
      na.rm = TRUE
    ) +
      1 #+ #c(rep(0,6))

    keyVarObs[1, keyVarObs[1, ] != -1] <- c(
      rep(0, 2),
      rep(1, 2),
      rep(2, 6),
      rep(3, 2)
    )
    keyVarObs[2, keyVarObs[2, ] != -1] <- max(keyVarObs[1, ]) + 1 #+  #c(rep(0,4),rep(1,8))
    keyVarObs[3, keyVarObs[3, ] != -1] <- max(keyVarObs[2, ]) + 1 #+  #c(rep(0,4),rep(1,6))

    obsCorStruct[2:3] <- 'AR'
    keyCorObs[2, is.na(keyCorObs[2, ])] <- c(rep(0, 7), rep(1, 4))

    keyLogFpar[2, keyLogFpar[2, ] != -1] <- c(0:4, rep(5, 7))
    keyLogFpar[3, keyLogFpar[3, ] != -1] <- max(keyLogFpar[2, ]) +
      1 +
      c(rep(0, 3), rep(1, 7))

    keyVarF[1, ] <- c(rep(0, 2), 1, rep(2, 9))
  })
}
