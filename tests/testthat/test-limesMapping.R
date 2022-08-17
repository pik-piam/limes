context("limesMapping")


test_that("Test if limesMapping creates a valid MagPie object", {

  regs = c("FI", "NO", "SE", "EE", "LV", "LT", "DK", "GB", "IE", "NL",
           "PL", "DE", "BE", "LU", "CZ", "SK", "AT", "CH", "HU", "RO", "SI",
           "FR", "HR", "BG", "IT", "ES", "PT", "GR", "Balkan")
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

  m_ok = new.magpie(regs,
                    years,
                    tau,
                    fill = (rep(seq(1, length(tau) * length(years)),
                                each = length(regs))
                            + rep(seq(0, length(regs) - 1),
                                  length(tau) * length(years))
                    )
  )


  expect_equal(limesMapping(m_test),
               m_ok)

})
