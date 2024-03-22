var ulNav = document.getElementsByClassName('dropdown-toggle');

for (var i = 0; i < ulNav.length; i++) {
  ulNav[i].addEventListener('click', function() {
    document.getElementById('tab-1216-5').style.display = 'none';
    document.getElementById('tab-1216-6').className = 'tab-pane.hide-tab';
    document.getElementById('tab-1216-6').style.display = 'none';
    prompt("EI!!");
  }); 
}

/*
document.getElementsByClassName('dropdown-toggle').addEventListener('click', function() {
  document.getElementById('tab-1216-6').class = 'tab-pane';
});
*/

$(document).ready(function() {
    // Remove os elementos <li> vazios da navbar
    $('.navbar-nav li a:empty').remove();
});
  