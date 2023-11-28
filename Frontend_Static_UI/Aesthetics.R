## Aestetics

dt_checkmark <- '<span class="glyphicon glyphicon-ok" style="color:deepskyblue"></span>'
dt_minus <- '<span class="glyphicon glyphicon-minus"></span>'
exclamation_span <- "<span id = '%s', class='glyphicon glyphicon-exclamation-sign'></span>"
blueq = icon("question-sign", lib="glyphicon", style = "color:deepskyblue;")
blueexcl = icon("exclamation-sign", lib="glyphicon", style = "color:deepskyblue;")
addTooltip_handler_script = "updateBoxCollapse = function(e) {let p = e; while (!p.classList.contains('collapse-box-group')) { if (p == document) return; p = p.parentNode}; Shiny.setInputValue('collapseTitleClick',{p:p.id, id: e.getAttribute('data-panel'), t: Date.now()});}; Shiny.addCustomMessageHandler('addPrompter', function(message) { target = document.getElementById(message.target); if (target != null) { if (target.classList.contains('glyphicon')) { target = target.parentElement;} target.className += message.attributes; target.setAttribute('aria-label', message.label); }})"
