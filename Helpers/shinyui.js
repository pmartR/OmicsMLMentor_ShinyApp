/* Manually set the text and loading bar progress of a fileInput.
id (str) the inputId of the fileInput
text_selector (str) jquery selector for the text portion of the fileInput
text (str) text to put inside the fileInput
enable (bool) whether to show the progress bar
*/
shinyjs.setFileInput = function(params) {
    var defaultParams = {
        id: null,
        text_selector: "",
        text: "",
        enable:true
    };
    params = shinyjs.getParams(params, defaultParams)
    
    var pbar = "#" + params.id + "_progress";
    var pbar_ = pbar + " .progress-bar";  
    
    if(params.enable){
        $(pbar).css("visibility", "visible");
        $(pbar_).css("width", "100%");
        $(params.text_selector).val(params.text);
        console.log("enabled")
    } else {
        $(pbar).css("visibility", "hidden");
        $(pbar_).css("width", "0%");
        $(params.text_selector).val("");
        console.log("disabled")
    }
}

shinyjs.isTabdisabled = function(el) {
    var tab = $('.nav li a[data-value=' + el + ']');
    var outstring = 'jscatch_disabled_' + el;
    Shiny.setInputValue(outstring, tab.hasClass('disabled'));
};

shinyjs.isIconhidden = function(el) {
  var icon = $('#' + el);
  var outstring = 'jscatch_icon_' + el;
  Shiny.setInputValue(outstring, icon.css('display') == 'none');
};

shinyjs.disableTab = function(params) {
  var defaultParams = {
    name: null,
    class:'disabled'
  };
  params = shinyjs.getParams(params, defaultParams);
  
  var tab = $('.nav li a[data-value=' + params.name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass(params.class);
};

shinyjs.enableTab = function(params) {
  var defaultParams = {
    name: null,
    class:'disabled'
  };
  params = shinyjs.getParams(params, defaultParams);
  
  var tab = $('.nav li a[data-value=' + params.name + ']');
  tab.unbind('click.tab');
  tab.removeClass(params.class);
};

shinyjs.disableBtn = function(selector, onoff) {
  $(selector).prop('disabled', onoff);
};

shinyjs.toggleTabInputs = function(params){
  var defaultParams = {
    tab_selector: null,
    sub_elements:'*',
    exclude_elements:null,
    disabled:true
  };
  params = shinyjs.getParams(params, defaultParams);

  $(params.tab_selector).find(params.sub_elements).not(params.exclude_elements).prop('disabled', params.disabled);
};