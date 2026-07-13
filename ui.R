indicator_info <- function(id, text) {
  tags$span(
    class = "indicator-info",
    tags$button(
      id = id,
      class = "indicator-info-button",
      type = "button",
      `aria-label` = text,
      `aria-describedby` = paste0(id, "_tooltip"),
      tags$i(class = "bi bi-info-circle", `aria-hidden` = "true")
    ),
    tags$span(
      id = paste0(id, "_tooltip"),
      class = "indicator-info-tooltip",
      role = "tooltip",
      text
    )
  )
}

ui <- fluidPage(
  
  useShinyjs(),
  theme = bs_theme(version = 5, base_font = font_google("Montserrat")),
  
  # Waiter loading screen
  use_waiter(),
  waiter_show_on_load(
    html = tagList(
      spin_1(),
      br(),
      h4("AQ Fund Dashboard Loading ...",
         style = "color:#FFFFFF; font-family:Montserrat;")
    ),
    color = "#39f"
  ),
  #color = "#002D72"
  tags$head(
    tags$style(HTML("
      .aq-welcome {
        position: fixed;
        right: 22px;
        bottom: 20px;
        width: 196px;
        height: 170px;
        z-index: 30000;
        opacity: 0;
        pointer-events: none;
        transform: translateY(16px) scale(0.96);
        transition: opacity 280ms ease, transform 280ms ease;
      }

      .aq-welcome.is-visible {
        opacity: 1;
        pointer-events: auto;
        transform: translateY(0) scale(1);
      }

      .aq-welcome-card {
        position: relative;
        width: 196px;
        height: 170px;
        background: transparent;
        border: 0;
        box-shadow: none;
        font-family: Montserrat, Arial, sans-serif;
      }

      .aq-welcome-close {
        position: absolute;
        top: 0;
        right: 0;
        width: 26px;
        height: 26px;
        display: inline-flex;
        align-items: center;
        justify-content: center;
        border: 0;
        background: transparent;
        color: #475569;
        border-radius: 50%;
        font-size: 15px;
        font-weight: 800;
        line-height: 1;
        cursor: pointer;
        opacity: 0.64;
      }

      .aq-welcome-close:hover,
      .aq-welcome-close:focus {
        color: #800000;
        opacity: 1;
        outline: none;
      }

      .aq-mascot {
        position: absolute;
        right: 8px;
        bottom: 2px;
        width: 136px;
        height: 136px;
        overflow: visible;
        filter: drop-shadow(0 12px 18px rgba(15, 23, 42, 0.22));
        animation: aq-mascot-float 3s ease-in-out infinite;
      }

      .aq-mascot .aq-eye {
        transform-box: fill-box;
        transform-origin: center;
        animation: aq-mascot-blink 4.8s infinite;
      }

      .aq-mascot .aq-arm {
        transform-box: fill-box;
        transform-origin: 10% 85%;
        animation: aq-mascot-wave 1.2s ease-in-out infinite;
      }

      .aq-mascot .aq-signal {
        transform-box: fill-box;
        transform-origin: center;
        animation: aq-mascot-pulse 2.4s ease-out infinite;
      }

      .aq-mascot-logo {
        fill: #006666;
        font: 900 12px Montserrat, Arial, sans-serif;
        letter-spacing: 0.03em;
      }

      .aq-speech {
        position: absolute;
        left: 10px;
        top: 22px;
        padding: 8px 14px;
        background: #800000;
        color: #ffffff;
        border-radius: 999px;
        font-size: 15px;
        font-weight: 900;
        letter-spacing: 0.01em;
        box-shadow: 0 8px 18px rgba(128, 0, 0, 0.22);
        animation: aq-speech-pop 2.8s ease-in-out infinite;
      }

      .aq-speech:before {
        content: '';
        position: absolute;
        left: 13px;
        bottom: -5px;
        width: 10px;
        height: 10px;
        background: #800000;
        transform: rotate(45deg);
      }

      @keyframes aq-mascot-float {
        0%, 100% { transform: translateY(0); }
        50% { transform: translateY(-7px); }
      }

      @keyframes aq-mascot-wave {
        0%, 100% { transform: rotate(-7deg); }
        50% { transform: rotate(-26deg); }
      }

      @keyframes aq-mascot-blink {
        0%, 92%, 100% { transform: scaleY(1); }
        95% { transform: scaleY(0.18); }
      }

      @keyframes aq-mascot-pulse {
        0% { opacity: 0.72; transform: scale(0.88); }
        70% { opacity: 0.1; transform: scale(1.18); }
        100% { opacity: 0; transform: scale(1.22); }
      }

      @keyframes aq-speech-pop {
        0%, 100% { transform: translateY(0) scale(1); }
        50% { transform: translateY(-3px) scale(1.04); }
      }

      @media (max-width: 640px) {
        .aq-welcome {
          right: 16px;
          bottom: 16px;
          width: 178px;
          height: 154px;
        }

        .aq-mascot {
          width: 122px;
          height: 122px;
        }
      }

      @media (prefers-reduced-motion: reduce) {
        .aq-welcome,
        .aq-speech,
        .aq-mascot,
        .aq-mascot .aq-arm,
        .aq-mascot .aq-eye,
        .aq-mascot .aq-signal {
          animation: none !important;
          transition: none !important;
        }
      }
    ")),
    tags$script(HTML("
      (function() {
        var key = 'aqfundWelcomeSeenMascotV2';

        function hasSeenWelcome() {
          try {
            return window.localStorage.getItem(key) === '1';
          } catch (error) {
            return false;
          }
        }

        function markWelcomeSeen() {
          try {
            window.localStorage.setItem(key, '1');
          } catch (error) {}
        }

        function hideWelcome() {
          var welcome = document.getElementById('aqWelcome');
          if (!welcome) return;

          welcome.classList.remove('is-visible');
          welcome.setAttribute('aria-hidden', 'true');

          window.setTimeout(function() {
            if (welcome && welcome.parentNode) {
              welcome.parentNode.removeChild(welcome);
            }
          }, 320);
        }

        function showWelcome() {
          var welcome = document.getElementById('aqWelcome');
          if (!welcome || hasSeenWelcome()) return;

          markWelcomeSeen();
          welcome.setAttribute('aria-hidden', 'false');
          welcome.classList.add('is-visible');

          window.setTimeout(hideWelcome, 7200);
        }

        document.addEventListener('DOMContentLoaded', function() {
          var close = document.getElementById('aqWelcomeClose');

          if (close) {
            close.addEventListener('click', hideWelcome);
          }

          document.addEventListener('keydown', function(event) {
            if (event.key === 'Escape') {
              hideWelcome();
            }
          });

          window.setTimeout(showWelcome, 3600);
        });
      })();
    "))
  ),

  tags$div(
    id = "aqWelcome",
    class = "aq-welcome",
    role = "status",
    `aria-live` = "polite",
    `aria-hidden` = "true",
    tags$div(
      class = "aq-welcome-card",
      tags$button(
        id = "aqWelcomeClose",
        class = "aq-welcome-close",
        type = "button",
        `aria-label` = "Close welcome",
        "x"
      ),
      tags$div(class = "aq-speech", "Welcome!"),
      tags$svg(
        class = "aq-mascot",
        xmlns = "http://www.w3.org/2000/svg",
        viewBox = "0 0 132 132",
        `aria-hidden` = "true",
        focusable = "false",
        tags$defs(
          tags$linearGradient(
            id = "aqBodyGradient",
            x1 = "36",
            y1 = "46",
            x2 = "104",
            y2 = "113",
            gradientUnits = "userSpaceOnUse",
            tags$stop(offset = "0%", `stop-color` = "#f8feff"),
            tags$stop(offset = "46%", `stop-color` = "#b7ebf1"),
            tags$stop(offset = "100%", `stop-color` = "#5cc4d6")
          ),
          tags$linearGradient(
            id = "aqGoldGradient",
            x1 = "56",
            y1 = "14",
            x2 = "83",
            y2 = "40",
            gradientUnits = "userSpaceOnUse",
            tags$stop(offset = "0%", `stop-color` = "#ffe9a6"),
            tags$stop(offset = "100%", `stop-color` = "#D4AF37")
          )
        ),
        tags$ellipse(
          cx = "70",
          cy = "124",
          rx = "43",
          ry = "9",
          fill = "rgba(15,23,42,0.16)"
        ),
        tags$circle(
          class = "aq-signal",
          cx = "70",
          cy = "23",
          r = "17",
          fill = "none",
          stroke = "#D4AF37",
          `stroke-width` = "3"
        ),
        tags$path(
          d = "M70 45V30",
          fill = "none",
          stroke = "#006666",
          `stroke-width` = "5",
          `stroke-linecap` = "round"
        ),
        tags$circle(
          cx = "70",
          cy = "22",
          r = "9",
          fill = "url(#aqGoldGradient)",
          stroke = "#ffffff",
          `stroke-width` = "3"
        ),
        tags$path(
          d = "M37 73C25 73 18 65 22 57",
          fill = "none",
          stroke = "#006666",
          `stroke-width` = "7",
          `stroke-linecap` = "round"
        ),
        tags$circle(
          cx = "22",
          cy = "57",
          r = "7",
          fill = "#D4AF37",
          stroke = "#ffffff",
          `stroke-width` = "2"
        ),
        tags$g(
          class = "aq-arm",
          tags$path(
            d = "M101 72C114 67 118 56 110 48",
            fill = "none",
            stroke = "#006666",
            `stroke-width` = "8",
            `stroke-linecap` = "round"
          ),
          tags$circle(
            cx = "110",
            cy = "48",
            r = "8",
            fill = "#D4AF37",
            stroke = "#ffffff",
            `stroke-width` = "2"
          )
        ),
        tags$rect(
          x = "49",
          y = "107",
          width = "14",
          height = "14",
          rx = "7",
          fill = "#006666"
        ),
        tags$rect(
          x = "77",
          y = "107",
          width = "14",
          height = "14",
          rx = "7",
          fill = "#006666"
        ),
        tags$rect(
          x = "35",
          y = "46",
          width = "69",
          height = "67",
          rx = "23",
          fill = "url(#aqBodyGradient)",
          stroke = "#006666",
          `stroke-width` = "3"
        ),
        tags$path(
          d = "M45 59C52 52 63 50 75 51",
          fill = "none",
          stroke = "#ffffff",
          `stroke-width` = "5",
          `stroke-linecap` = "round",
          opacity = "0.72"
        ),
        tags$circle(
          class = "aq-eye",
          cx = "59",
          cy = "75",
          r = "5.5",
          fill = "#111827"
        ),
        tags$circle(
          class = "aq-eye",
          cx = "81",
          cy = "75",
          r = "5.5",
          fill = "#111827"
        ),
        tags$circle(
          cx = "50",
          cy = "84",
          r = "4",
          fill = "#ffffff",
          opacity = "0.58"
        ),
        tags$circle(
          cx = "90",
          cy = "84",
          r = "4",
          fill = "#ffffff",
          opacity = "0.58"
        ),
        tags$path(
          d = "M60 89C64 95 75 95 80 89",
          fill = "none",
          stroke = "#111827",
          `stroke-width` = "3.5",
          `stroke-linecap` = "round"
        ),
        tags$text(
          "AQ",
          x = "70",
          y = "105",
          class = "aq-mascot-logo",
          `text-anchor` = "middle"
        ),
        tags$path(
          d = "M43 116H96",
          fill = "none",
          stroke = "#ffffff",
          `stroke-width` = "3",
          `stroke-linecap` = "round",
          opacity = "0.46"
        )
      )
    )
  ),

  # CSS: Small Button Styling
  tags$head(tags$style(HTML("
    .btn-sm {
      padding: 3px 10px;
      font-size: 12px;
    }
  "))),
  
  # CSS: Responsive Two-Column Layout
  tags$style(HTML("
    .responsive-two-column {
      display: flex;
      flex-direction: row;
      gap: 20px;
    }
    .responsive-two-column > div {
      flex: 1;
    }
    @media (max-width: 768px) {
      .responsive-two-column {
        flex-direction: column;
      }
    }
  ")),
  
  # CSS: Navigation Menu (Desktop and Mobile)
  tags$style(HTML("
    .mobile-menu {
      display: flex;
      flex-direction: column;
      position: absolute;
      top: 100%;
      left: 0;
      width: 100%;
      background-color: #800000;
      text-align: center;
      padding: 0;
      z-index: 10000;
      transform: translateY(-100%);
      opacity: 0;
      transition: transform 0.4s ease, opacity 0.4s ease;
      pointer-events: none;
    }
    .mobile-menu.show {
      transform: translateY(0%);
      opacity: 1;
      pointer-events: auto;
    }
    .mobile-menu a {
      color: white;
      padding: 12px 0;
      display: block;
      font-weight: 500;
      font-size: 16px;
      text-decoration: none;
      border-bottom: 1px solid rgba(255, 255, 255, 0.1);
    }
    .mobile-menu-icon {
      display: none;
      cursor: pointer;
    }
    @media (max-width: 768px) {
      .desktop-menu { display: none !important; }
      .mobile-menu-icon { display: block; }
    }
    .desktop-menu a:hover,
    .mobile-menu a:hover {
      color: gold !important;
    }
  ")),
  
  # CSS: Dropdown and Checkbox Styling
  tags$style(HTML("
    #level_1_id + .dropdown-toggle,
    #level_2_id + .dropdown-toggle {
      background: orange !important;
      color: #fff !important;
      font-size: 14px !important;
      padding: 2px 4px !important;
      height: 28px !important;
    }
    #use_state, #use_district_id {
      width: 16px !important;
      height: 15px !important;
      transform: scale(0.8);
      margin-right: 4px;
    }
    #use_state + label,
    #use_district_id + label {
      font-size: 7px !important;
    }
  ")),
  # CSS: Modal Styling
  tags$style(HTML("
    .modal-dialog.modal-balanced {
      max-width: 1000px;
      width: 90%;
      margin: 30px auto;
    }
    .modal-content {
      border-radius: 8px;
      box-shadow: 0 4px 30px rgba(0,0,0,0.2);
      overflow: hidden;
    }
    .modal-header {
      background-color: #8ed2ff;
      border-bottom: 1px solid #ccc;
      padding: 15px;
    }
    .modal-title {
      font-weight: bold;
      font-size: 22px;
      color: #2c3e50;
    }
    .modal-body {
      padding: 20px;
      max-height: 60vh;
      overflow-y: auto;
      overflow-x: auto;
    }
    .modal-footer {
      background-color: #f9f9f9;
      padding: 10px 15px;
      border-top: 1px solid #ccc;
    }
    .close { font-size: 18px; }
  ")),
  
  ######## Value Box
  # CSS: Enhanced Value Boxes
  tags$style(HTML("
    .value-box {
      border-radius: 12px;
      padding: 20px 22px;
      color: white;
      box-shadow: 0 6px 20px rgba(0, 0, 0, 0.18);
      height: 100%;
      position: relative;
      overflow: hidden;
      transition: all 0.3s ease;
    }
    .value-box:hover {
      transform: translateY(-5px);
      box-shadow: 0 10px 25px rgba(0, 0, 0, 0.25);
    }
    .value-box-title {
      font-size: 14.5px;
      font-weight: 500;
      margin-bottom: 6px;
      opacity: 0.95;
      letter-spacing: 0.3px;
    }
    .value-box-value {
      font-size: 32px;
      font-weight: 700;
      line-height: 1.05;
      margin: 0;
    }
    .value-box-icon {
      font-size: 48px;
      opacity: 0.18;
      position: absolute;
      top: 18px;
      right: 20px;
      transition: all 0.3s ease;
    }
    .value-box:hover .value-box-icon {
      opacity: 0.28;
      transform: scale(1.08);
    }
    .value-box small {
      font-size: 15px;
      font-weight: 500;
    }
  ")),
  ###################
  tags$head(tags$link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.1/font/bootstrap-icons.css")),
  
  # Header
  tags$div(
    style = "background-color: #800000; color: white; padding: 8px 20px; display: flex; justify-content: space-between; flex-wrap: wrap;",
    tags$div(
      style = "display: flex; align-items: center;",
      tags$img(src = "https://res.cloudinary.com/diwsbenwr/image/upload/v1750129379/uchicago_logo_cbiopy.png", height = "25px"),
      tags$span("THE UNIVERSITY OF CHICAGO", style = "margin-left: 10px; font-weight: bold;")
    ),
    tags$div("EPIC · UCHICAGO CLIMATE & GROWTH")
  ),
  
  tags$style(HTML("
[id$='level_1_id'] + .bootstrap-select .dropdown-toggle,
[id$='level_2_id'] + .bootstrap-select .dropdown-toggle {
    height: 28px !important;
    min-height: 28px !important;
    padding: 2px 8px !important;
    font-size: 12px !important;
}
")),
  tags$style(HTML("
  .kpi-strip {
    display: grid;
    grid-template-columns: repeat(4, 1fr);
    gap: 14px;
    margin-bottom: 26px;
  }

  .kpi-card {
    background: #ffffff;
    border: 1px solid #e5e7eb;
    border-left: 5px solid var(--accent);
    padding: 16px 18px;
    min-height: 112px;
    min-width: 0;
    display: flex;
    flex-direction: column;
    justify-content: space-between;
  }

  .kpi-top {
    display: flex;
    justify-content: space-between;
    align-items: flex-start;
    gap: 12px;
  }

  .kpi-title {
    font-size: 12px;
    font-weight: 800;
    color: #64748b;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    line-height: 1.3;
  }

  .indicator-info {
    position: relative;
    display: inline-flex;
    margin-left: 5px;
    vertical-align: -1px;
  }

  .indicator-info-button {
    width: 20px;
    height: 20px;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    padding: 0;
    border: 0;
    background: transparent;
    color: #64748b;
    font-size: 14px;
    line-height: 1;
    cursor: help;
  }

  .indicator-info-button:hover,
  .indicator-info-button:focus-visible {
    color: var(--accent);
    outline: none;
  }

  .indicator-info-button:focus-visible {
    box-shadow: 0 0 0 2px #ffffff, 0 0 0 4px var(--accent);
    border-radius: 50%;
  }

  .indicator-info-tooltip {
    position: absolute;
    top: calc(100% + 8px);
    left: 0;
    z-index: 50;
    width: 250px;
    padding: 10px 12px;
    background: #111827;
    color: #ffffff;
    border-radius: 6px;
    box-shadow: 0 8px 22px rgba(15, 23, 42, 0.24);
    font-size: 12px;
    font-weight: 600;
    line-height: 1.45;
    letter-spacing: 0;
    text-align: left;
    text-transform: none;
    opacity: 0;
    visibility: hidden;
    pointer-events: none;
    transform: translateY(-3px);
    transition: opacity 140ms ease, transform 140ms ease, visibility 140ms ease;
  }

  .indicator-info:hover .indicator-info-tooltip,
  .indicator-info:focus-within .indicator-info-tooltip {
    opacity: 1;
    visibility: visible;
    transform: translateY(0);
  }

  .kpi-card:nth-child(n+3) .indicator-info-tooltip {
    right: 0;
    left: auto;
  }

  .kpi-card:hover,
  .kpi-card:focus-within {
    position: relative;
    z-index: 20;
  }

  .plot-title-info {
    display: inline-flex;
    align-items: center;
    gap: 7px;
    position: relative;
    letter-spacing: 0;
  }

  .plot-info {
    position: relative;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 20px;
    height: 20px;
    padding: 0;
    border: 0;
    background: transparent;
    color: #64748b;
    font: inherit;
    cursor: help;
    outline: none;
  }

  .plot-info-icon {
    width: 17px;
    height: 17px;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    border: 1.5px solid currentColor;
    border-radius: 50%;
    font-family: Arial, sans-serif;
    font-size: 11px;
    font-weight: 800;
    font-style: normal;
    line-height: 1;
  }

  .plot-info:hover,
  .plot-info:focus-visible,
  .plot-info:focus-within {
    color: #800000;
  }

  .plot-info:focus-visible {
    border-radius: 50%;
    box-shadow: 0 0 0 2px #ffffff, 0 0 0 4px #800000;
  }

  .plot-info-tooltip {
    position: absolute;
    top: calc(100% + 8px);
    left: 50%;
    z-index: 40;
    width: 290px;
    padding: 10px 12px;
    background: #111827;
    color: #ffffff;
    border-radius: 6px;
    box-shadow: 0 8px 22px rgba(15, 23, 42, 0.24);
    font-family: Montserrat, Arial, sans-serif;
    font-size: 12px;
    font-weight: 600;
    line-height: 1.45;
    letter-spacing: 0;
    text-align: left;
    white-space: normal;
    opacity: 0;
    visibility: hidden;
    pointer-events: none;
    transform: translate(-50%, -3px);
    transition: opacity 140ms ease, transform 140ms ease, visibility 140ms ease;
  }

  .plot-info--axis .plot-info-tooltip {
    top: auto;
    bottom: calc(100% + 8px);
    transform: translate(-50%, 3px);
  }

  .plot-info:hover .plot-info-tooltip,
  .plot-info:focus-within .plot-info-tooltip,
  .plot-info.is-open .plot-info-tooltip {
    opacity: 1;
    visibility: visible;
    transform: translate(-50%, 0);
  }

  .kpi-icon {
    font-size: 20px;
    color: var(--accent);
    opacity: 0.9;
  }

  .kpi-value {
    font-size: 30px;
    font-weight: 900;
    color: #111827;
    line-height: 1;
    margin-top: 12px;
    word-break: break-word;
  }

  .kpi-subtitle {
    font-size: 12px;
    color: #64748b;
    margin-top: 6px;
    font-weight: 600;
  }

  @media (max-width: 1200px) {
    .kpi-strip {
      grid-template-columns: repeat(2, 1fr);
    }
  }

  @media (max-width: 768px) {
    .kpi-strip {
      grid-template-columns: repeat(1, 1fr);
    }
  }
")),
  tags$head(
    tags$script(HTML("
      document.addEventListener('click', function (event) {
        var button = event.target.closest('.plot-info');

        if (!button) {
          document.querySelectorAll('.plot-info.is-open').forEach(function (item) {
            item.classList.remove('is-open');
            item.setAttribute('aria-expanded', 'false');
          });
          return;
        }

        event.preventDefault();
        event.stopPropagation();

        document.querySelectorAll('.plot-info.is-open').forEach(function (item) {
          if (item !== button) {
            item.classList.remove('is-open');
            item.setAttribute('aria-expanded', 'false');
          }
        });

        var isOpen = button.classList.toggle('is-open');
        button.setAttribute('aria-expanded', isOpen ? 'true' : 'false');
      }, true);

      document.addEventListener('keydown', function (event) {
        if (event.key !== 'Escape') return;

        document.querySelectorAll('.plot-info.is-open').forEach(function (item) {
          item.classList.remove('is-open');
          item.setAttribute('aria-expanded', 'false');
        });
      });
    "))
  ),
  
  tags$style(HTML("
  .highcharts-heatmap-series .highcharts-point {
    stroke-width: 1px !important;
    transition: none !important;
  }

  .highcharts-heatmap-series .highcharts-point-hover {
    stroke-width: 1px !important;
    stroke: #ffffff !important;
    filter: none !important;
    transform: none !important;
  }

  .highcharts-heatmap-series .highcharts-point:hover {
    stroke-width: 1px !important;
    stroke: #ffffff !important;
    filter: none !important;
    transform: none !important;
  }

  .highcharts-point-hover {
    filter: none !important;
    transform: none !important;
  }
")),
  
  tags$style(HTML("
  .map-action-row {
    display: flex;
    gap: 10px;
    align-items: center;
    margin: 10px 0 12px 0;
    flex-wrap: wrap;
  }

  .btn-map-action {
    background: #ffffff !important;
    color: #111827 !important;
    border: 1px solid #d1d5db !important;
    border-radius: 10px !important;
    padding: 8px 14px !important;
    font-size: 13px !important;
    font-weight: 700 !important;
    box-shadow: 0 1px 2px rgba(15, 23, 42, 0.06) !important;
  }

  .btn-map-action:hover {
    background: #f8fafc !important;
    border-color: #94a3b8 !important;
  }

  .map-helper-text {
    font-size: 12px;
    color: #64748b;
    font-weight: 600;
    margin-left: 4px;
  }
")),
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;500;600;700;800&display=swap"
    )
  ),
  # Navigation Bar
  tags$div(
    style = "background-color: white; padding: 10px 30px; display: flex; justify-content: space-between; align-items: center; border-bottom: 1px solid #ccc; position: relative;",
    
    tags$div(tags$img(src = "https://res.cloudinary.com/diwsbenwr/image/upload/v1776844394/aqfundv23_eoieot.png", height = "50px")),
    
    tags$div(
      class = "desktop-menu",
      style = "display: flex; gap: 25px; font-size: 16px; font-weight: 500;",
      
      tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('tab_selected', 'news')",
        style = "color: grey; text-decoration: none;",
        tags$i(class = "bi bi-graph-up-arrow", style = "margin-right: 6px;"),
        "GIS"
      ),
      
      tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('tab_selected', 'index')",
        style = "color: grey; text-decoration: none;",
        tags$i(class = "bi bi-broadcast-pin", style = "margin-right: 6px;"),
        "Monitoring Gaps"
      ),
      
      tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('tab_selected', 'impacts')",
        style = "color: grey; text-decoration: none;",
        tags$i(class = "bi bi-graph-up-arrow", style = "margin-right: 6px;"),
        "Trend analysis"
      )
      
      # tags$a("Country", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'about')", style = "color: grey; text-decoration: none;"),
      # tags$a("Country Capital", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'popcal')", style = "color: grey; text-decoration: none;"),
      # 
      # tags$a("GBD", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'facts')", style = "color: grey; text-decoration: none;"),
      # tags$a("Factsheet", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'reports')", style = "color: grey; text-decoration: none;"),
      # tags$a("About", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'news')", style = "color: grey; text-decoration: none;")
    ),
    
    tags$div(
      style = "display: flex; gap: 10px; align-items: center;",
      tags$img(src = "https://img.icons8.com/ios-filled/20/search--v1.png"),
      tags$img(
        class = "mobile-menu-icon",
        src = "https://img.icons8.com/ios-filled/20/menu.png",
        onclick = "var menu = document.getElementById('mobileTabs'); console.log('Toggling menu:', menu); menu.classList.toggle('show');"
      )
    ),
    
    tags$div(
      id = "mobileTabs",
      class = "mobile-menu",
      tags$a("GIS", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'news')"),
      tags$a("Monitoring Gaps", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'index')"),
      tags$a("Trend analysis", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'impacts')"),
     # tags$a("GIS", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'news')")
      
      # tags$a("Country", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'about')"),
      # tags$a("Country Capital", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'popcal')"),
      # tags$a("GBD", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'facts')"),
      # tags$a("Factsheet", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'reports')"),
      # tags$a("About", href = "#", onclick = "Shiny.setInputValue('tab_selected', 'news')")
    )
  ),
  
  ####### End
  # Main content
  tags$div(
    id = "main-content",
    style = "padding: 30px;",
    
    # INDEX SECTION with FILTERS
    hidden(tags$div(
      id = "index_section",
      # h2("The Index"),
      fluidRow(
        
        
       # mod_filter_ui("dropdown_filter",data = final_data)),
      
      # ==================== PROFESSIONAL & COLORFUL VALUE BOXES ====================
      
      # Filters inside Index Section
      
      br(),
     

      fluidRow(
          column(
            width = 6,
        #    wellPanel(
              checkboxGroupInput(
                inputId = "year_heatmp",
                label = "Select Year",
                choices = 2025:2026,
                selected = 2025,
                inline = TRUE
              )
              
        
        #    )
          )
          
        ),
br(),
br(),
      fluidRow(
        column(
          width = 6,
          wellPanel(
            highcharter::highchartOutput("funding_priority_bubble", height = "480px")
          )
        ),

        column(
          width = 6,
          wellPanel(
            # checkboxGroupInput(
            #   inputId = "year_heatmp",
            #   label = "Select Year",
            #   choices = 2025:2026,
            #   selected = 2025,
            #   inline = TRUE
            # ),
            
            highcharter::highchartOutput("country_month_heatmap", height = "630px")
            
                      )
        )
      ),
      br(),
br(),
      fluidRow(
        column(
          width = 6,
          wellPanel(
            highcharter::highchartOutput(
              "active_sensor_drilldown",
              height = "540px"
            )
          )
        )
      ),
      
      br()
      
      # fluidRow(
      # 
      #   highchartOutput("districtt_wise_trend")
      #  # leafletOutput("gis_check")
      # 
      # )
      
    ))
    ),
    
    # Other hidden sections
    
    
    
    
    hidden(tags$div(id = "impacts_section", 
                    
                    
                    
                    fluidRow(
                      
                      column(
                        3,
                        pickerInput(
                          inputId = ("country_trd"),
                          label = "Select Country",
                          choices = sort(unique(openaq_month_trend_c$name0)),
                          multiple = FALSE,
                          selected = "Gambia",#sort(unique(openaq_month_trend_c$name0))[1],
                          options = list(
                            `actions-box` = TRUE,
                            size = 6,
                            title = "Select Country",
                            `none-selected-text` = "Country Not Found",
                            `live-search` = TRUE,
                            liveSearchPlaceholder = "Country"
                          )
                        )
                      ),
                      
                      column(
                        3,
                        pickerInput(
                          inputId = ("year_trd"),
                          label = "Select Year",
                          choices = NULL,
                          multiple = TRUE,
                          selected = NULL,
                          options = list(
                            `actions-box` = TRUE,
                            size = 6,
                            title = "Select Year",
                            `selected-text-format` = "count > 2",
                            `count-selected-text` = "{0} Years Selected",
                            `none-selected-text` = "Year Not Found",
                            `select-all-text` = "All",
                            `live-search` = TRUE,
                            liveSearchPlaceholder = "Year"
                          )
                        )
                      )
                    ),
                    
                    fluidRow(
                      
                    column(6,
                             wellPanel(
                      highchartOutput("avg_aqi_id", height = "450px")
                             )),
                      
                    column(6,
                           wellPanel(
                             fluidRow(
                               column(2,
                                      checkboxInput("use_state", "State", value = FALSE)
                                      # checkboxInput("use_district", "District(s)", value = FALSE)
                               ),
                               
                               column(2,
                                      #  checkboxInput("use_state", "State(s)", value = FALSE),
                                      checkboxInput("use_district_id", "District", value = FALSE)
                               ),
                               column(4,
                                      conditionalPanel(
                                        condition ="input.use_state == true",
                                        pickerInput(
                                          inputId = "state_trd",
                                          choices = "",
                                          multiple = FALSE,
                                          #  label = "Select State:",
                                          options = list(
                                            `actions-box` = TRUE,
                                            size = 6,
                                            title = "Select State(s):",
                                            `selected-text-format` = "count > 0",
                                            `count-selected-text` = "{0} State Selected",
                                            `none-selected-text` = "State Not Found",
                                            `select-all-text` = "All",
                                            `live-search` = TRUE,
                                            liveSearchPlaceholder = "State"
                                          ),
                                          selected = ""  # This sets country as default
                                        )
                                      )),
                               
                               
                               column(4,
                                      conditionalPanel(
                                        condition = "input.use_district_id == true",
                                        pickerInput(
                                          inputId = "district_trd",
                                          choices = "",
                                          multiple = FALSE,
                                          #label = "Select District(s):",
                                          options = list(
                                            `actions-box` = TRUE,
                                            size = 6,
                                            # title = "Select Districts",
                                            `selected-text-format` = "count > 0",
                                            `count-selected-text` = "{0} Districts Selected",
                                            `none-selected-text` = "Districts Not Found",
                                            `select-all-text` = "All",
                                            `live-search` = TRUE,
                                            liveSearchPlaceholder = "Districts"
                                          ),
                                          selected = ""  # This sets country as default
                                        )
                                      ))),
                             
                             highchartOutput("monthly_pm25_glm", height = "440px")
                           )
                    )
                      ),

                    # fluidRow(
                    #   column(6,
                    #          wellPanel(
                    #            fluidRow(
                    #              column(2,
                    #                     checkboxInput("use_state_aq", "State", value = FALSE)
                    #                     # checkboxInput("use_district", "District(s)", value = FALSE)
                    #              ),
                    #              
                    #              column(2,
                    #                     #  checkboxInput("use_state", "State(s)", value = FALSE),
                    #                     checkboxInput("use_district_id_aq", "District", value = FALSE)
                    #              ),
                    #              column(4,
                    #                     conditionalPanel(
                    #                       condition ="input.use_state_aq == true",
                    #                       pickerInput(
                    #                         inputId = "state_trd_aq",
                    #                         choices = "",
                    #                         multiple = FALSE,
                    #                         #  label = "Select State:",
                    #                         options = list(
                    #                           `actions-box` = TRUE,
                    #                           size = 6,
                    #                           title = "Select State(s):",
                    #                           `selected-text-format` = "count > 0",
                    #                           `count-selected-text` = "{0} State Selected",
                    #                           `none-selected-text` = "State Not Found",
                    #                           `select-all-text` = "All",
                    #                           `live-search` = TRUE,
                    #                           liveSearchPlaceholder = "State"
                    #                         ),
                    #                         selected = ""  # This sets country as default
                    #                       )
                    #                     )),
                    #              
                    #              
                    #              column(4,
                    #                     conditionalPanel(
                    #                       condition = "input.use_district_id_aq == true",
                    #                       pickerInput(
                    #                         inputId = "district_trd_aq",
                    #                         choices = "",
                    #                         multiple = FALSE,
                    #                         #label = "Select District(s):",
                    #                         options = list(
                    #                           `actions-box` = TRUE,
                    #                           size = 6,
                    #                           # title = "Select Districts",
                    #                           `selected-text-format` = "count > 0",
                    #                           `count-selected-text` = "{0} Districts Selected",
                    #                           `none-selected-text` = "Districts Not Found",
                    #                           `select-all-text` = "All",
                    #                           `live-search` = TRUE,
                    #                           liveSearchPlaceholder = "Districts"
                    #                         ),
                    #                         selected = ""  # This sets country as default
                    #                       )
                    #                     ))),
                    #            
                    #            highchartOutput("annual_pm25_location_drilldown", height = "440px")
                    #          )
                    #   )
                    #   
                    #   # column(
                    #   #   12,
                    #   #   wellPanel(
                    #   #     highchartOutput(
                    #   #       "annual_pm25_location_drilldown",
                    #   #       height = "600px"
                    #   #     )
                    #   #   )
                    #   # )
                    # ),
                    
                   # mod_filter_trend_ui(id = "trend_filter", data = map_city_gis),
                    
                    br()
                    
)),
    
    (tags$div(id = "news_section",               
                    # includeHTML("www/about_aqli.html")
                    
                    fluidRow(
                      
                      column(
                        3,
                        
                        pickerInput(
                          inputId = ("country_gis"),
                          label = "Select Country",
                          choices = unique(openaq_data_district$name0),
                          multiple = F,
                          
                          options = list(
                            `actions-box` = TRUE,
                            size = 6,
                            title = "Select Country",
                            `selected-text-format` = "count > 2",
                            `count-selected-text` = "{0} Country Selected",
                            `none-selected-text` = "Country Not Found",
                            `select-all-text` = "All",
                            `live-search` = TRUE,
                            liveSearchPlaceholder = "Country"
                          ),
                          
                          selected = "Nepal",#unique(openaq_data_district$name0)
                        )),
                      
                      column(
                        3,  
                        pickerInput(
                          inputId = ("state_gis"),
                          label = "Select State(s):",
                          choices = " ",# unique(data$name1),
                          multiple = TRUE,
                          
                          options = list(
                            `actions-box` = TRUE,
                            size = 6,
                            title = "Select State",
                            `selected-text-format` = "count > 2",
                            `count-selected-text` = "{0} State Selected",
                            `none-selected-text` = "State Not Found",
                            `select-all-text` = "All",
                            `live-search` = TRUE,
                            liveSearchPlaceholder = "State"
                          ),
                          
                          selected = "" #unique(data$name1)[1]
                        )),
                      
                      # column(
                      #   3,
                      #   pickerInput(
                      #     inputId = ("district_gis"),
                      #     label = "Select District(s):",
                      #     choices = " ",#unique(data$name2),
                      #     multiple = TRUE,
                      #     
                      #     options = list(
                      #       `actions-box` = TRUE,
                      #       size = 6,
                      #       title = "Select District",
                      #       `selected-text-format` = "count > 2",
                      #       `count-selected-text` = "{0} District Selected",
                      #       `none-selected-text` = "District Not Found",
                      #       `select-all-text` = "All",
                      #       `live-search` = TRUE,
                      #       liveSearchPlaceholder = "District"
                      #     ),
                      #     
                      #     selected = "" #unique(data$name2)[1]
                      #   )
                      # ),
                      
                      column(
                        3,
                        
                        pickerInput(
                          inputId = ("year_gis"),
                          label = "Select Year",
                          choices = "",
                          multiple = FALSE,
                          selected = "",
                          
                          options = list(
                            `actions-box` = TRUE,
                            size = 6,
                            title = "Select Year",
                            `selected-text-format` = "count > 2",
                            `count-selected-text` = "{0} Year Selected",
                            `none-selected-text` = "Year Not Found",
                            `select-all-text` = "All",
                            `live-search` = TRUE,
                            liveSearchPlaceholder = "Year"
                          )
                        )
                      )
                    ),
                    # =========================================================
                    # CSS: Add once inside UI / dashboardBody
                    # =========================================================
                    

                    
                    div(
                      id = "value_boxes",
                      class = "kpi-strip",
                      
                      # div(
                      #   class = "kpi-card",
                      #   style = "--accent:#800000;",
                      #   div(
                      #     class = "kpi-top",
                      #     div(class = "kpi-title", "Countries Covered"),
                      #     tags$i(class = "bi bi-globe-americas kpi-icon")
                      #   ),
                      #   div(
                      #     div(class = "kpi-value", textOutput("vb_countries", inline = TRUE)),
                      #     div(class = "kpi-subtitle", "AQ Fund countries with monitoring data")
                      #   )
                      # ),
                      
                      div(
                        class = "kpi-card",
                        style = "--accent:#D4AF37;",
                        div(
                          class = "kpi-top",
                          div(
                            class = "kpi-title",
                            "Active Sensors",
                            indicator_info(
                              "info_active_sensors",
                              "Number of unique ground-monitoring sensors reporting data for the selected country, states, and year."
                            )
                          ),
                          tags$i(class = "bi bi-broadcast-pin kpi-icon")
                        ),
                        div(
                          div(class = "kpi-value", textOutput("vb_sensor", inline = TRUE)),
                          div(class = "kpi-subtitle", "Ground monitors reporting")
                        )
                      ),
                      
                      div(
                        class = "kpi-card",
                        style = "--accent:#006666;",
                        div(
                          class = "kpi-top",
                          div(
                            class = "kpi-title",
                            "Owner Groups",
                            indicator_info(
                              "info_owner_groups",
                              "Number of distinct awardee or monitoring-network groups represented in the selected data."
                            )
                          ),
                          tags$i(class = "bi bi-diagram-3 kpi-icon")
                        ),
                        div(
                          div(class = "kpi-value", textOutput("vb_owner", inline = TRUE)),
                          div(class = "kpi-subtitle", "Awardee / monitoring networks")
                        )
                      ),
                      
                      div(
                        class = "kpi-card",
                        style = "--accent:#e53935;",
                        div(
                          class = "kpi-top",
                          div(
                            class = "kpi-title",
                            "Average PM₂.₅",
                            indicator_info(
                              "info_average_pm25",
                              "Mean fine-particle concentration across the selected ground-monitoring readings. Higher values indicate more air pollution."
                            )
                          ),
                          tags$i(class = "bi bi-cloud-haze2-fill kpi-icon")
                        ),
                        div(
                          div(
                            class = "kpi-value",
                            textOutput("vb_avg_pm", inline = TRUE),
                            tags$span(" µg/m³", style = "font-size:14px; font-weight:800; color:#64748b;")
                          ),
                          div(class = "kpi-subtitle", "Across the monitored network")
                        )
                      ),
                      
                      div(
                        class = "kpi-card",
                        style = "--accent:#2E8B57;",
                        div(
                          class = "kpi-top",
                          div(
                            class = "kpi-title",
                            "Data Coverage",
                            indicator_info(
                              "info_data_coverage",
                              "Average percentage of expected observations reported by the selected sensors. A higher percentage means more complete data."
                            )
                          ),
                          tags$i(class = "bi bi-check-circle kpi-icon")
                        ),
                        div(
                          div(
                            class = "kpi-value",
                            textOutput("vb_data_cov", inline = TRUE),
                            tags$span("%", style = "font-size:14px; font-weight:800; color:#64748b;")
                          ),
                          div(class = "kpi-subtitle", "Average reporting completeness")
                        )
                      )
                    ),
                    
                   
                    
                    
                   
              
              fluidRow(
                column(
                  5,
                  radioGroupButtons(
                    inputId = "switch_btn",
                    label = "Select Map View:",
                    choices = setNames(
                      c("pm25", "llpp"),
                      c(
                        HTML("Ground Monitoring PM<sub>2.5</sub>"),
                        HTML("Satellite PM<sub>2.5</sub>")
                      )
                    ),
                    selected = "pm25",
                    justified = TRUE,
                    checkIcon = list(yes = icon("check"))
                  )
                )
              ),
              
              conditionalPanel(
                condition = "input.switch_btn == 'llpp'",
                
                fluidRow(
                  column(
                    3,
                    pickerInput(
                      inputId = "year_gis_aqli",
                      label = "Select Year",
                      choices = 1998:2024,
                      multiple = FALSE,
                      selected = 2024,
                      options = list(
                        `actions-box` = TRUE,
                        size = 6,
                        title = "Select Year",
                        `none-selected-text` = "Year Not Found",
                        `live-search` = TRUE,
                        liveSearchPlaceholder = "Year"
                      )
                    )
                  )
                )
              ),
              
              fluidRow(
                column(
                  5,
                  wellPanel(
                    
                    div(
                      class = "map-action-row",
                      
                      actionButton(
                        "load_district_layer",
                        "Load Districts",
                        class = "btn-map-action"
                      ),
                      
                      actionButton(
                        "load_sensor_layer",
                        "Load Sensors",
                        class = "btn-map-action"
                      ),
                      
                      actionButton(
                        "clear_map_layers",
                        "Clear Extra Layers",
                        class = "btn-map-action"
                      ),
                      
                      span(
                        class = "map-helper-text",
                        "State layer loads first. Select fewer states before loading districts for large countries."
                      )
                    ),
                    
                    leafletOutput("country_wise_pm", height = "440px")
                  )
                ),
                
                column(
                  7,
                  wellPanel(
                    reactableOutput("gis_table", height = "530px")
                  )
                )
              )
                      
                      
                   #   mod_filter_gis_ui("dropdown_filter_gis",data = openaq_data))
                    
                    
    )
    #print("ui done ---------------------------")
  )
)
)

# ui <- secure_app(ui,
#                  fab_button( position =  "bottom-right", label = "Logout"),
#                  tags_top =
#                    tags$div(
#                     # tags$h4("AQLI Dashboard", style = "align:center"),
#                      tags$img(
#                        src = "epic-aqli-logo.png", width = 400
#                      )
#                    ),
#                  #background  = "linear-gradient( rgba(99, 203, 255,0), rgba(99, 203, 255,1)),url('https://i.ibb.co/CWcsc4m/brlps-wall.png');"
#                  #background  = "linear-gradient( rgba(99, 203, 255,0), rgba(99, 203, 255,1)),url('https://s3.gifyu.com/images/brlps_tattva_new_gif.gif');"
#                                              
#                  # style = sprintf(backgroundImageCSS,  "https://images.plot.ly/language-icons/api-home/r-logo.png")
#                  background = "
#                             radial-gradient(circle at top left, #002D72, #F26B38);
#                             background-size: cover;
#                           ",
#                                          
#                  tags_bottom = tags$div(
#                    tags$p(
#                      "For any assistance, please  contact ",
#                      tags$a(
#                        href = "mailto:guptap@uchicago.edu?Subject=AQLI%20Dashboard",
#                        target="_top", "administrator"
#                      )
#                    )
#                  )
#                  
#                  
#                  
# )
