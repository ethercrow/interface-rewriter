#import "{{ master.vName }}.h"

@implementation {{ master.vName }}

- (id)initWithFrame:(CGRect)frame
{
    if (!(self = [super initWithFrame:frame])) {
        return nil;
    }

{{#subviews}}
    {{subviewClass}} *{{subviewName}} = [[{{subviewClass}} alloc] initWithFrame:CGRectMake({{subviewFrame.rectX}}, {{subviewFrame.rectY}}, {{subviewFrame.rectW}}, {{subviewFrame.rectH}})];
    [{{subviewParent}} addSubview:{{subviewName}}];

{{/subviews}}

    [self setNeedsLayout];

    return self;
}

- (void)layoutSubviews
{
{{#layoutUpdaters}}
    // OHAI
{{/layoutUpdaters}}
}

@end

