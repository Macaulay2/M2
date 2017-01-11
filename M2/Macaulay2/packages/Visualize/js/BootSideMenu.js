(function ( $ ) {

	$.fn.BootSideMenu = function( options ) {

		var oldCode, newCode, side;

		newCode = "";

		var settings = $.extend({
			side:"left",
			autoClose:true
		}, options );

		side = settings.side;
		autoClose = settings.autoClose;

		this.addClass("container sidebar");

		if(side=="left"){
			this.addClass("sidebar-left");
		}else if(side=="right"){
			this.addClass("sidebar-right");
		}else{
			this.addClass("sidebar-left");
		}

		oldCode = this.html();

		newCode += "<div class=\"row\">\n";
		newCode += "	<div class=\"col-xs-12 col-sm-12 col-md-12 col-lg1-12\" data-side=\""+side+"\">\n"+ oldCode+" </div>\n";
		newCode += "</div>";
		newCode += "<div class=\"toggler\">\n";
		newCode += "	<span class=\"glyphicon glyphicon-chevron-right\">&nbsp;</span> <span class=\"glyphicon glyphicon-chevron-left\">&nbsp;</span>\n";
		newCode += "</div>\n";

		this.html(newCode);

		if(autoClose){
			$(this).find(".toggler").trigger("click");
		}

	};

	$(document).on('click', '.sidebar .list-group-item', function(){
		$('.sidebar .list-group-item').each(function(){
			$(this).removeClass('active');
		});
		//$(this).addClass('active');
	});


	$(document).on('click', '.sidebar .list-group-item', function(event){
		var idToToggle, this_offset, this_x, this_y, href, side;
		event.preventDefault();
		href = $(this).attr('href');

		if(href.substr(0,1)=='#'){

			idToToggle = href.substr(1,href.length);

			if(searchSubMenu(idToToggle)){

				this_offset = $(this).offset();
				side = $(this).parent().parent().attr('data-side');

				if(side=='left'){
					this_x = $(this).width() + 10;
					this_y = this_offset.top +1;
					$('#'+idToToggle).css('left', this_x);
					$('#'+idToToggle).css('top', this_y);
				}else if(side=='right'){
					this_x = $(this).width()+10;
					this_y = this_offset.top +1;
					$('#'+idToToggle).css('right', this_x);
					$('#'+idToToggle).css('top', this_y);
				}

				$('#'+idToToggle).fadeIn();

			}else{
				$('.submenu').fadeOut();
			}
		}
	});


	$(document).on('click','.toggler', function(){
		var toggler = $(this);
		var container = toggler.parent();
		var listaClassi = container[0].classList;
		var side = getSide(listaClassi);
		var containerWidth = container.width();
		var status = container.attr('data-status');
		if(!status){
			status = "opened";
		}
		doAnimation(container, containerWidth, side, status);
	});

	/*Cerca un div con classe submenu e id uguale a quello passato*/
	function searchSubMenu(id){
		var found = false;
		$('.submenu').each(function(){
			var thisId = $(this).attr('id');
			if(id==thisId){
				found = true;
			}
		});
		return found;
	}

//restituisce il lato del sidebar in base alla classe che trova settata
function getSide(listaClassi){
	var side;
	for(var i = 0; i<listaClassi.length; i++){
		if(listaClassi[i]=='sidebar-left'){
			side = "left";
			break;
		}else if(listaClassi[i]=='sidebar-right'){
			side = "right";
			break;
		}else{
			side = null;
		}
	}
	return side;
}
//esegue l'animazione
function doAnimation(container, containerWidth, sidebarSide, sidebarStatus){
	var toggler = container.children()[1];
	if(sidebarStatus=="opened"){
		if(sidebarSide=="left"){
			container.animate({
				left:-(containerWidth+2)
			});
			toggleArrow(toggler, "left");
		}else if(sidebarSide=="right"){
			container.animate({
				right:- (containerWidth +2)
			});
			toggleArrow(toggler, "right");
		}
		container.attr('data-status', 'closed');
	}else{
		if(sidebarSide=="left"){
			container.animate({
				left:0
			});
			toggleArrow(toggler, "right");
		}else if(sidebarSide=="right"){
			container.animate({
				right:0
			});
			toggleArrow(toggler, "left");
		}
		container.attr('data-status', 'opened');

	}

}

function toggleArrow(toggler, side){
	if(side=="left"){
		$(toggler).children(".glyphicon-chevron-right").css('display', 'block');
		$(toggler).children(".glyphicon-chevron-left").css('display', 'none');
	}else if(side=="right"){
		$(toggler).children(".glyphicon-chevron-left").css('display', 'block');
		$(toggler).children(".glyphicon-chevron-right").css('display', 'none');
	}
}

}( jQuery ));
