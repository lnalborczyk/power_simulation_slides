# install.packages("gifski")
set.seed(666)

n_reps <- 40L
true_d <- 0.5
alpha <- 0.05
n_per_group <- c(20L, 80L)

# Each repetition is a new two-group experiment
p_values <- sapply(n_per_group, function (n) {
    
    replicate(n_reps, {
        control <- rnorm(n, mean = 0, sd = 1)
        treatment <- rnorm(n, mean = true_d, sd = 1)
        t.test(treatment, control, var.equal = TRUE)$p.value
        })
    
})

power <- vapply(n_per_group, function (n) {
    
    power.t.test(
        n = n, delta = true_d, sd = 1, sig.level = alpha,
        type = "two.sample", alternative = "two.sided"
        )$power
    
    }, numeric(1) )

col_success <- "#287f80"
col_failure <- "#c46e65"
col_ink <- "#1c5253"

# Faces are drawn as vector graphics, avoiding emoji/font compatibility problems
draw_face <- function (cx, cy, significant) {
    
    face_colour <- if (significant) "#d4eee6" else "#f9ded9"
    stroke <- if (significant) col_success else col_failure
    symbols(
        cx, cy, circles = 4.0, inches = FALSE,
        add = TRUE, bg = face_colour, fg = stroke, lwd = 2
        )
    points(cx + c(-1.35, 1.35), rep(cy + 1.0, 2), pch = 16, cex = 1)
    u <- seq(-2, 2, length.out = 50)
    mouth_y <- if (significant) cy - 1.65 + 0.25 * u^2 else cy - 0.7 - 0.25 * u^2
    lines(cx + u, mouth_y, col = col_ink, lwd = 2.8)
    
}

draw_card <- function (left, n, target_power, p, frame, heading) {
    
    rect(left, 10, left + 55, 61, col = "#f7faf8", border = "#d5e5de", lwd = 2)
    text(left + 4, 57, heading, col = col_ink, font = 2, cex = 1.25, adj = 0)
    text(left + 4, 52, sprintf("n = %d / group   |   power ≈ %.0f%%", n, 100 * target_power), col = "#506b69", cex = 0.93, adj = 0)
    
    for (study in seq_len(n_reps) ) {
        
        row <- (study - 1L) %/% 8L
        column <- (study - 1L) %% 8L
        x <- left + 4 + 4.7 * column
        y <- 44.5 - 5.3 * row
        fill <- if (study > frame) "#edf1ef" else if (p[study] < alpha) col_success else col_failure
        rect(x, y - 3.9, x + 3.9, y, col = fill, border = if (study > frame) "#e2e9e5" else "white", lwd = 1.1)
        }
    
    this_hit <- p[frame] < alpha
    draw_face(left + 48, 39.5, this_hit)
    text(left + 48, 32, sprintf("p = %.3f", p[frame]), cex = 1.0, col = col_ink, font = 2)
    text(left + 48, 27.8, if (this_hit) "p < .05" else "p ≥ .05", cex = 0.90, col = if (this_hit) col_success else col_failure, font = 2)
    text(left + 4, 14.8, sprintf("%d / %d significant results", sum(p[seq_len(frame)] < alpha), frame), cex = 0.95, col = col_ink, adj = 0)

}

# gifski automatically captures each base-R plot as a separate GIF frame
dir.create("figures", showWarnings = FALSE, recursive = TRUE)

gifski::save_gif({
    for (frame in seq_len(n_reps) ) {
      
        par(mar = rep(0, 4), bg = "white", family = "sans")
        
        plot.new()
        
        plot.window(xlim = c(0, 120), ylim = c(0, 72), asp = 1, xaxs = "i", yaxs = "i")
        
        text(
            6, 64.5,
            "Same true effect (d = 0.5); independent experiments; two-sided t tests (alpha = .05)",
            adj = 0, cex = 0.93, col = "#536b6c"
            )
    
        draw_card(5,  n_per_group[1], power[1], p_values[, 1], frame, "LOW POWER")
        draw_card(63, n_per_group[2], power[2], p_values[, 2], frame, "HIGHER POWER")
    
        rect(6, 5.1, 8.5, 7.6, col = col_success, border = NA)
        text(9.8, 6.3, "Significant (p < .05)", adj = 0, col = col_ink, cex = 0.90)
        rect(38, 5.1, 40.5, 7.6, col = col_failure, border = NA)
        text(41.8, 6.3, "Non-significant (p ≥ .05)", adj = 0, col = col_ink, cex = 0.90)
        
        text(
            115, 6.3, sprintf("Replication %d / %d", frame, n_reps),
            adj = 1, col = "#536b6c", cex = 0.90
            )
        
    }}, gif_file = "figures/why_power.gif", width = 1667, height = 1000,
    delay = 0.5, loop = TRUE, progress = TRUE, res = 200
    )

message ("Saved: figures/why_power.gif")
