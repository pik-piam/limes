context("limesMapping")


test_that("Test if limesMapping creates a valid MagPie object", {

  regs = c(FIN = "FI", NOR = "NO", SWE = "SE",
           EST = "EE", LVA = "LV", LTU = "LT",
           DNK = "DK", GBR = "GB", IRL = "IE",
           NLD = "NL", POL = "PL", DEU = "DE",
           BEL = "BE", LUX = "LU", CZE = "CZ",
           SVK = "SK", AUT = "AT", CHE = "CH",
           HUN = "HU", ROM = "RO", SVN = "SI",
           FRA = "FR", HRV = "HR", BAL = "Balkan",
           BGR = "BG", ITA = "IT", ESP = "ES",
           PRT = "PT", GRC = "GR")
  tau = c(1,2)
  years = c(2005, 2010)
  full_names = paste(rep(tau, length(regs)),
                     rep(regs, each = length(tau)),
                     sep = ".")

  size_array = length(full_names) * length(years)

  m_test = new.magpie("GLO",
                      years,
                      full_names,
                      fill = seq(1, size_array))

  m_ok = new.magpie(names(regs),
                    years,
                    tau,
                    fill = (rep(seq(1, length(tau) * length(years)),
                                each = length(regs))
                            + rep(seq(0, length.out = length(regs), by = 4),
                                  length(tau) * length(years))
                    )
  )


  expect_equal(limesMapping(m_test),  m_ok)

})
