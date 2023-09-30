using UnityEditor;
using UnityEngine;

public class RenderCubemapWizard : ScriptableWizard
{
    public Camera camera;

    private void OnWizardUpdate()
    {
        isValid = (camera != null);
    }

    private void OnWizardCreate()
    {
        Cubemap cubemap = new Cubemap(512, TextureFormat.ARGB32, false);
        camera.RenderToCubemap(cubemap);
        AssetDatabase.CreateAsset(cubemap, $"Assets/Shaders/InteriorMapping/Cubemaps/{camera.name}.cubemap");
    }

    [MenuItem("Tools/Cubemap Wizard")]
    static void RenderCubemap()
    {
        DisplayWizard<RenderCubemapWizard>("Render Cuvemap", "Render");
    }
}
